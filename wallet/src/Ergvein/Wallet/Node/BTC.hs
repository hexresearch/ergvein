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

import Control.Concurrent (forkIO)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM
import Data.ByteString (ByteString)
import Data.Serialize (decode, runGet, runPut)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Data.Word
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Socket (AddrInfo(..), getNameInfo, NameInfoFlag(..), SockAddr(..)
  , getAddrInfo, defaultHints, AddrInfoFlag(..), SocketType(..), Family(..))
import Reflex
import Reflex.Dom
import Reflex.ExternalRef
import Servant.Client(BaseUrl(..), showBaseUrl)
import UnliftIO hiding (atomically)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Node.Socket

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

  let net = btc
      nodeLog :: MonadIO m => Text -> m ()
      nodeLog = logWrite . (nodeString BTC url <>)
  (restartE, fireRestart)   <- newTriggerEvent
  (reqE, fireReq)           <- newTriggerEvent
  (closeE, fireClose)       <- newTriggerEvent
  (closeSE, fireCloseS)     <- newTriggerEvent
  buildE                    <- getPostBuild
  socadrs                   <- liftIO $ toSockAddr url
  statRef                   <- newExternalRef nstat
  case socadrs of
    [] -> do
      nodeLog $ "Failed to convert BaseUrl to SockAddr: " <> T.pack (showBaseUrl url)
      pure NodeConnection {
          nodeconCurrency   = BTC
        , nodeconUrl        = url
        , nodeconStatus     = statRef
        , nodeconOpensE     = never
        , nodeconCloseE     = closeE
        , nodeconCloseFire  = fireCloseS ()
        , nodeconRestart    = fireRestart ()
        , nodeconReqFire    = fireReq
        , nodeconRespE      = never
        , nodeconExtra      = ()
        , nodeconIsUp       = pure False
        }
    sa :_ -> do
      let startE = leftmost [buildE, restartE]

      -- Resolve address
      peerE <- performFork $ ffor startE $ const $ do
        (Just sname, Just sport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True sa
        pure $ Peer sname sport

      -- Start the connection
      s <- fmap switchSocket $ widgetHold (pure noSocket) $ ffor peerE $ \peer -> do
        socket SocketConf {
            _socketConfPeer   = peer
          , _socketConfSend   = runPut . putMessage net <$> reqE
          , _socketConfPeeker = peekMessage net url
          , _socketConfClose  = closeSE
          , _socketConfReopen = Just 10
          }

      let respE = _socketInbound s
      -- performEvent_ $ ffor respE $ nodeLog . showt
      performEvent_ $ ffor (_socketRecvEr s) $ nodeLog . showt
      -- Start the handshake
      performEvent_ $ ffor (socketConnected s) $ const $ liftIO $ fireReq =<< mkVers net sa

      -- Finalize the handshake by sending "verack" message as a response
      -- Also, respond to ping messages by corrseponding pongs
      performEvent_ $ ffor respE $ \case
        MVersion Version{..} -> liftIO $ do
          nodeLog $ "Received version at height: " <> showt startHeight
          fireReq MVerAck
        MPing (Ping v) -> liftIO $ fireReq $ MPong (Pong v)
        _ -> pure ()

      -- Track handshake status
      let verAckE = fforMaybe respE $ \case
            MVerAck -> Just True
            _ -> Nothing

      shakeD <- holdDyn False $ leftmost [verAckE, False <$ closeE]
      let openE = fmapMaybe (\b -> if b then Just () else Nothing) $ updated shakeD

      pure $ NodeConnection {
        nodeconCurrency   = BTC
      , nodeconUrl        = url
      , nodeconStatus     = statRef
      , nodeconOpensE     = openE
      , nodeconCloseE     = closeE
      , nodeconCloseFire  = fireCloseS ()
      , nodeconRestart    = fireRestart ()
      , nodeconReqFire    = fireReq
      , nodeconRespE      = respE
      , nodeconExtra      = ()
      , nodeconIsUp       = shakeD
      }

-- | Internal peeker to parse messages coming from peer.
peekMessage :: (MonadPeeker m, MonadIO m, MonadThrow m, PlatformNatives)
    => Network
    -> BaseUrl
    -> m Message
peekMessage net url = do
  x <- peek 24
  case decode x of
    Left e -> do
      nodeLog $ "Consumed " <> showt (B.length x)
      nodeLog $ "Could not decode incoming message header: " <> showt e
      nodeLog $ showt x
      throwM DecodeHeaderError
    Right (MessageHeader !_ !cmd !len !_) -> do
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
  where nodeLog = logWrite . (nodeString BTC url <>)

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
    go = fmap (fmap addrAddress) . liftIO $
      getAddrInfo
        (Just defaultHints {
            addrFlags = [AI_ADDRCONFIG]
          , addrSocketType = Stream
          , addrFamily = AF_INET
          }) (Just baseUrlHost) (Just (show baseUrlPort))
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
