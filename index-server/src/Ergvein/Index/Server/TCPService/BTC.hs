module Ergvein.Index.Server.TCPService.BTC
  (
    BtcSocket(..)
  , connectBtc
  , getIncChannel
  , requestBlock
  , dummyBtcSock
  ) where

import Control.Concurrent.Lifted (fork, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Unlift
import Control.Monad.Random (randomIO)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Serialize (decode, runGet, runPut)
import Data.Time.Clock.POSIX
import Data.Word
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Haskoin.Block

import Ergvein.Index.Server.TCPService.Socket
import Ergvein.Text

import qualified Control.Exception.Safe as Ex
import qualified Data.ByteString as B
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NSB

data BtcSocket = BtcSocket {
  btcSockNetwork  :: !Network
, btcSockAddr     :: !N.SockAddr
, btcSockRecv     :: !(TChan Message)
, btcSockSend     :: Message -> IO ()
, btcSockIsActive :: !(TVar Bool)
, btcSockOnActive :: !(TChan Bool)
}

dummyBtcSock :: (MonadIO m, Ex.MonadMask m, MonadBaseControl IO m) => Network -> m BtcSocket
dummyBtcSock net = do
  sa       <- liftIO $ (N.addrAddress . head) <$> N.getAddrInfo (Just hints) (Just "localhost") (Just "8080")
  shakeVar <- liftIO $ newTVarIO False
  incChan  <- liftIO newBroadcastTChanIO
  openChan <- liftIO newBroadcastTChanIO
  pure $ BtcSocket {
    btcSockNetwork  = net
  , btcSockAddr     = sa
  , btcSockRecv     = incChan
  , btcSockSend     = const $ pure ()
  , btcSockIsActive = shakeVar
  , btcSockOnActive = openChan
  }
  where
    hints :: N.AddrInfo
    hints = N.defaultHints
      { N.addrFlags = [N.AI_ADDRCONFIG]
      , N.addrSocketType = N.Stream }

getIncChannel :: MonadIO m => BtcSocket -> m (TChan Message)
getIncChannel = liftIO . atomically . dupTChan . btcSockRecv

connectBtc :: (MonadIO m, Ex.MonadMask m, MonadBaseControl IO m)
  => Network    -- ^ Btc network
  -> N.HostName -- ^ Server "host"name or IP address.
  -> N.ServiceName -- ^ Server service port name or number.
  -> TVar Bool
  -> m BtcSocket
connectBtc net host port closeVar = do
  sa       <- liftIO $ (N.addrAddress . head) <$> N.getAddrInfo (Just hints) (Just host) (Just port)
  incChan  <- liftIO newBroadcastTChanIO
  onActive <- liftIO newBroadcastTChanIO
  sendChan <- liftIO newTChanIO
  intVar   <- liftIO $ newTVarIO False
  shakeVar <- liftIO $ newTVarIO False
  let btcsock = BtcSocket {
          btcSockNetwork  = net
        , btcSockAddr     = sa
        , btcSockRecv     = incChan
        , btcSockSend     = atomically . writeTChan sendChan
        , btcSockIsActive = shakeVar
        , btcSockOnActive = onActive
        }

  closeChan <- liftIO newTChanIO
  fork $ liftIO $ fix $ \next -> do
    b <- atomically $ readTVar closeVar
    if b
      then atomically $ writeTChan closeChan $ Ex.SomeException PeerSeppuku
      else threadDelay 1000000 >> next
  fork $ fix $ \next -> do
    connect host port $ \(sock, sockaddr) -> liftIO $ do
      atomically $ writeTVar shakeVar False
      let env = PeekerEnv intVar sock
      let inFire = atomically . writeTChan incChan
      let readErFire = atomically . writeTChan closeChan

      fork $ forever $ do
        msg <- atomically $ readTChan sendChan
        NSB.send sock $ runPut . putMessage net $ msg

      fork $ fix $ \next -> do
        mma <- Ex.tryAny $ Ex.try $ runReaderT (peekMessage net sa) env
        case mma of
          Left e -> readErFire e >> next
          Right (Left (e :: ReceiveException)) -> readErFire $ Ex.SomeException e
          Right (Right a) -> inFire a >> next
      fork $ performHandshake btcsock
      fork $ btcPinger btcsock
      print =<< atomically (readTChan closeChan)
      liftIO $ N.close sock
    b <- liftIO $ readTVarIO closeVar
    if b then liftIO $ putStrLn "Close connection to BTC node" else next
  pure btcsock
  where
    hints :: N.AddrInfo
    hints = N.defaultHints
      { N.addrFlags = [N.AI_ADDRCONFIG]
      , N.addrSocketType = N.Stream }

performHandshake :: MonadIO m => BtcSocket -> m ()
performHandshake btcs@BtcSocket{..} = liftIO $ do
  ic <- getIncChannel btcs
  btcSockSend =<< mkVers btcSockNetwork btcSockAddr
  fix $ \next -> do
    msg <- atomically $ readTChan ic
    case msg of
      MVersion Version{..} -> do
        print $ "Received version at height: " <> showt startHeight
        btcSockSend MVerAck
      _ -> next
  fix $ \next -> do
    msg <- atomically $ readTChan ic
    case msg of
      MVerAck -> do
        atomically $ writeTVar btcSockIsActive True
        atomically $ writeTChan btcSockOnActive True
      _ -> next

btcPinger :: MonadIO m => BtcSocket -> m ()
btcPinger btcs@BtcSocket{..} = liftIO $ do
  ic <- getIncChannel btcs
  forever $ do
    msg <- atomically $ readTChan ic
    case msg of
      MVersion Version{..} -> do
        print $ "Received version at height: " <> showt startHeight
        btcSockSend MVerAck
      _ -> pure ()

requestBlock :: MonadIO m => BtcSocket -> BlockHash -> m Block
requestBlock btcs@BtcSocket{..} bh = liftIO $ do
  ic <- getIncChannel btcs
  let req = MGetData $ GetData $ [InvVector InvBlock $ getBlockHash bh]
  tid <- fork $ forever $ do
    btcSockSend req
    threadDelay 1000000
  blk <- fix $ \next -> do
    msg <- atomically $ readTChan ic
    case msg of
      MBlock blk -> if bh == headerHash (blockHeader blk)
        then pure blk
        else next
      _ -> next
  killThread tid
  pure blk

-- | Create version data message
mkVers :: MonadIO m => Network -> N.SockAddr -> m Message
mkVers net url = liftIO $ do
  now   <- round <$> getPOSIXTime
  nonce <- randomIO
  pure $ MVersion $ Version
    { version = 70012
    , services = 0
    , timestamp = now
    , addrRecv = NetworkAddress 0 (sockToHostAddress url)
    , addrSend = NetworkAddress 0 (sockToHostAddress $ N.SockAddrInet 0 0)
    , verNonce = nonce
    , userAgent = VarString (getHaskoinUserAgent net)
    , startHeight = 0
    , relay = True
    }

-- | Internal peeker to parse messages coming from peer.
peekMessage :: (MonadPeeker m, MonadIO m, MonadThrow m)
    => Network
    -> N.SockAddr
    -> m Message
peekMessage net url = do
  x <- peek 24
  case decode x of
    Left e -> do
      nodeLog $ "Consumed " <> showt (B.length x)
      nodeLog $ "Could not decode incoming message header: " <> showt e
      nodeLog $ showt x
      throwM DecodeHeaderError
    Right (MessageHeader !_ !_ !len !_) -> do
      -- nodeLog $ showt cmd
      when (len > 32 * 2 ^ (20 :: Int)) $ do
        nodeLog "Payload too large"
        throwM (PayloadTooLarge len)
      let parseMessage bs = case runGet (getMessage net) bs of
            Left e -> do
              nodeLog $ "Cannot decode payload: " <> showt e
              throwM CannotDecodePayload
            Right !msg -> pure msg
      if len == 0 then parseMessage x else do
        y <- peek (fromIntegral len)
        parseMessage $ x `B.append` y
  where nodeLog = liftIO . print

-- | Reasons why a peer may stop working.
data PeerException
    = PeerMisbehaving !String
      -- ^ peer is being a naughty boy
    | DuplicateVersion
      -- ^ peer sent an extra version message
    | DecodeHeaderError
      -- ^ incoming message headers could not be decoded
    | CannotDecodePayload
      -- ^ incoming message payload could not be decoded
    | PeerIsMyself
      -- ^ nonce for peer matches ours
    | PayloadTooLarge !Word32
      -- ^ message payload too large
    | PeerAddressInvalid
      -- ^ peer address not valid
    | PeerSentBadHeaders
      -- ^ peer sent wrong headers
    | NotNetworkPeer
      -- ^ peer cannot serve block chain data
    | PeerNoSegWit
      -- ^ peer has no segwit support
    | PeerTimeout
      -- ^ request to peer timed out
    | UnknownPeer
      -- ^ peer is unknown
    | PeerTooOld
      -- ^ peer has been connected too long
    | PeerSeppuku
      -- ^ peer has been asked to kill itself by it's master
    deriving (Eq, Show)
instance Ex.Exception PeerException
