{-
  Implementation of BTC connector
-}
module Ergvein.Wallet.Node.BTC
  (
    BTCType(..)
  , NodeBTC
  , initBTCNode
  , commandToText
  ) where

import Conduit
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import Data.ByteString (ByteString)
import Data.Conduit.Network (AppData(..), clientSettings, runGeneralTCPClient, appSource, appSink)
import Data.Serialize (decode, runGet, runPut)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Data.Word
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Socket
import Reflex
import Servant.Client(BaseUrl(..), showBaseUrl)
import UnliftIO hiding (atomically)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.Prim

-- These two are for dummy stats
import Control.Monad.Random
import Ergvein.Text

data BTCType = BTCType
type NodeBTC t = NodeConnection t BTCType

instance CurrencyRep BTCType where
  curRep _ = BTC

-- | TODO: Change this once actual connection is implemented
instance HasNode BTCType where
  type NodeReq BTCType = Message
  type NodeResp BTCType = Message
  type NodeSpecific BTCType = ()

initBTCNode :: MonadBaseConstr t m => BaseUrl -> m (NodeBTC t)
initBTCNode url = do

  -- Dummy status TODO: Make status real later
  b  <- liftIO randomIO
  d :: Double <- liftIO $ randomRIO (0, 1.5)
  bh <- liftIO randomIO
  let nstat = if b then Nothing else Just $ NodeStatus bh (realToFrac d)


  let net = btcTest   -- TODO: Switch to a proper BTC network
  buildE              <- getPostBuild
  reqChanIn           <- liftIO $ newBroadcastTChanIO
  reqChanOut          <- liftIO $ atomically $ dupTChan reqChanIn
  (respE, fireResp)   <- newTriggerEvent
  (reqE, fireReq)     <- newTriggerEvent
  (closeE, fireClose) <- newTriggerEvent
  socadrs             <- liftIO $ toSockAddr url

  let externalClose :: IO () -- This closure is exposed to the wallet to manually kill the connection
      externalClose = atomically . writeTChan reqChanIn $ MsgKill

  -- Send incoming messages to the channel
  performEvent_ $ (liftIO . atomically . writeTChan reqChanIn . MsgMessage) <$> reqE

  -- Start the connection
  performEvent $ ffor buildE $ const $ liftIO $ case socadrs of
    []   -> logWrite $ "Failed to convert BaseUrl to SockAddr: " <> T.pack (showBaseUrl url)
    sa:_ -> do
      -- Spawn connection listener
      forkIO $ (runNode url net reqChanOut fireResp) `catch` \(e :: PeerException) -> do
        logWrite $ nodeString url <> "Halting the connection: " <> showt e
        fireClose ()

      -- Start the handshake
      atomically . writeTChan reqChanIn . MsgMessage =<< (mkVers net sa)

  -- Finalize the handshake by sending "verack" message as a response
  performEvent_ $ ffor respE $ \case
    MVersion Version{..} -> do
      logWrite $ nodeString url <> "Received version at height: " <> showt startHeight
      liftIO $ atomically . writeTChan reqChanIn $ MsgMessage MVerAck
    _ -> pure ()

  pure $ NodeConnection {
    nodeconCurrency = BTC
  , nodeconUrl      = url
  , nodeconStatus   = nstat
  , nodeconOpensE   = () <$ respE
  , nodeconCloseEF  = (closeE, externalClose)
  , nodeconReqFire  = fireReq
  , nodeconRespE    = respE
  , nodeExtra       = ()
  }

-- | Connect to a socket via TCP.
withConnection :: MonadUnliftIO m => BaseUrl -> (AppData -> m a) -> m a
withConnection BaseUrl{..} f =
  let cset = clientSettings baseUrlPort (cs baseUrlHost)
  in runGeneralTCPClient cset f

-- | A process that runs the connection and handles messages
-- Does not perform the handshake!
runNode :: (MonadUnliftIO m, MonadIO m, PlatformNatives)
  => BaseUrl                      -- Node's url
  -> Network                      -- Which network to use: BTC or BTCTest
  -> TChan MsgWrap                -- Request channel
  -> (Message -> IO ())           -- Response fire
  -> m ()
runNode u net incChan fire = withConnection u $ \ad -> peer_session ad
  where
    go = forever $ dispatchMessage =<< liftIO (atomically (readTChan incChan))
    peer_session ad =
        let ins = appSource ad
            ons = appSink ad
            src = runConduit $ ins .| inPeerConduit net u .| mapM_C send_msg
            snk = outPeerConduit net .| ons
         in withAsync src $ \as -> do
                link as
                runConduit (go .| snk)
    send_msg = liftIO . fire

-- | Wrapper for message dispatching
data MsgWrap = MsgMessage Message | MsgKill

-- | Message dispatcher to distinguish between messages from a peer and external messages
dispatchMessage :: MonadIO m => MsgWrap -> ConduitT i Message m ()
dispatchMessage (MsgMessage msg) = yield msg
dispatchMessage MsgKill = throwIO PeerSeppuku

-- | Internal conduit to parse messages coming from peer.
inPeerConduit :: (MonadIO m, PlatformNatives)
    => Network
    -> BaseUrl
    -> ConduitT ByteString Message m ()
inPeerConduit net a = forever $ do
    x <- takeCE 24 .| foldC
    case decode x of
        Left e -> do
          logWrite $ nodeString a <> "Could not decode incoming message header"
          throwIO DecodeHeaderError
        Right (MessageHeader _ cmd len _) -> do
          when (len > 32 * 2 ^ (20 :: Int)) $ do
            logWrite $ nodeString a <> "Payload too large"
            throwIO $ PayloadTooLarge len
          y <- takeCE (fromIntegral len) .| foldC
          case runGet (getMessage net) $ x `B.append` y of
              Left e -> do
                logWrite $ nodeString a <> "Cannot decode payload: " <> showt e
                throwIO CannotDecodePayload
              Right msg -> yield msg

-- | Outgoing peer conduit to serialize and send messages.
outPeerConduit :: Monad m => Network -> ConduitT Message ByteString m ()
outPeerConduit net = awaitForever $ yield . runPut . putMessage net

-- | Create version data message
mkVers :: MonadIO m => Network -> SockAddr -> m Message
mkVers net url = liftIO $ do
  now   <- round <$> getPOSIXTime
  nonce <- randomIO
  pure $ MVersion $ Version
    { version = 70012
    , services = 0
    , timestamp = now
    , addrRecv = NetworkAddress 0 url
    , addrSend = NetworkAddress 0 (SockAddrInet 0 0)
    , verNonce = nonce
    , userAgent = VarString (getHaskoinUserAgent net)
    , startHeight = 0
    , relay = True
    }

-- | Resolve a host and port to a list of 'SockAddr'. May do DNS lookups.
toSockAddr :: MonadUnliftIO m => BaseUrl -> m [SockAddr]
toSockAddr BaseUrl{..} = go `catch` e
  where
    go =
        fmap (fmap addrAddress) . liftIO $
        getAddrInfo
            (Just
                 defaultHints
                 { addrFlags = [AI_ADDRCONFIG]
                 , addrSocketType = Stream
                 , addrFamily = AF_INET
                 })
            (Just baseUrlHost)
            (Just (show baseUrlPort))
    e :: Monad m => SomeException -> m [SockAddr]
    e _ = return []

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
instance Exception PeerException

commandToText :: MessageCommand -> Text
commandToText = either (const "unknown") id . TE.decodeUtf8' . commandToString

-- | Node string for logging
nodeString :: BaseUrl -> Text
nodeString BaseUrl{..} = "[BTC]<" <> T.pack baseUrlHost <> ":" <> showt baseUrlPort <> ">: "
