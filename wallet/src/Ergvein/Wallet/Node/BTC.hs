{-
  Implementation of BTC connector
-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Node.BTC
  (
    BTCType(..)
  , NodeBTC
  , initBTCNode
  , commandToText
  ) where

import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Serialize (decode, runGet, runPut)
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Data.Word
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Socket (getNameInfo, NameInfoFlag(..), SockAddr(..))
import Reflex
import Reflex.Dom
import Reflex.Network
import Reflex.ExternalRef
import UnliftIO hiding (atomically)

import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Sepulcas.Native
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Node.Socket
import Ergvein.Wallet.Platform

-- These two are for dummy stats
import Control.Monad.Random
import Ergvein.Text

instance CurrencyRep BTCType where
  curRep _ = BTC

instance HasNode BTCType where
  type NodeReq BTCType = Message
  type NodeResp BTCType = Message
  type NodeSpecific BTCType = ()

initBTCNode :: (MonadBaseConstr t m, MonadHasSettings t m) => Bool -> SockAddr -> Event t NodeMessage -> m (NodeBTC t)
initBTCNode doLog sa msgE = do
  -- Dummy status TODO: Make status real later
  b  <- liftIO randomIO
  d :: Double <- liftIO $ randomRIO (0, 1.5)
  bh <- liftIO randomIO
  let nstat = if b then Nothing else Just $ NodeStatus bh (realToFrac d)

  let net = btcNetwork
      nodeLog :: MonadIO m => Text -> m ()
      nodeLog = if doLog then logWrite . (nodeString BTC sa <>) else const (pure ())

  let restartE = fforMaybe msgE $ \case
        NodeMsgRestart -> Just ()
        _ -> Nothing
      closeE = fforMaybe msgE $ \case
        NodeMsgClose -> Just ()
        _ -> Nothing
      reqE = fforMaybe msgE $ \case
        NodeMsgReq (NodeReqBTC req) -> Just req
        _ -> Nothing
  buildE                    <- getPostBuild
  statRef                   <- newExternalRef nstat
  let startE = leftmost [buildE, restartE]

  -- Resolve address
  peerE <- performFork $ ffor startE $ const $ do
    (Just sname, Just sport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True sa
    pure $ Peer sname sport

  proxyD <- getSocksConf
  rec
    -- Start the connection
    s <- fmap switchSocket $ networkHold (pure noSocket) $ ffor peerE $ \peer -> do
      socket SocketConf {
          _socketConfPeer   = peer
        , _socketConfSend   = fmap (runPut . putMessage net) $ leftmost [reqE, handshakeE, hsRespE]
        , _socketConfPeeker = peekMessage net sa
        , _socketConfClose  = closeE
        , _socketConfReopen = Just (1, 2) -- reconnect after 1 seconds 2 retries
        , _socketConfProxy  = proxyD
        }

    let respE = _socketInbound s
    -- Start the handshake
    handshakeE <- performEvent $ ffor (socketConnected s) $ const $ mkVers net sa
    -- Finalize the handshake by sending "verack" message as a response
    -- Also, respond to ping messages by corrseponding pongs
    hsRespE <- performEvent $ fforMaybe respE $ \case
      MVersion Version{..} -> Just $ liftIO $ do
        nodeLog $ "Received version at height: " <> showt startHeight
        pure MVerAck
      MPing (Ping v) -> Just $ pure $ MPong (Pong v)
      _ -> Nothing
    -- End rec

  performEvent_ $ ffor (_socketRecvEr s) $ nodeLog . showt

  -- Track handshake status
  let verAckE = fforMaybe respE $ \case
        MVerAck -> Just True
        _ -> Nothing

  shakeD <- holdDyn False $ leftmost [verAckE, False <$ closeE]
  let openE = fmapMaybe (\o -> if o then Just () else Nothing) $ updated shakeD
      closedE = () <$ _socketClosed s
  pure $ NodeConnection {
    nodeconCurrency   = BTC
  , nodeconUrl        = sa
  , nodeconStatus     = statRef
  , nodeconOpensE     = openE
  , nodeconCloseE     = closedE
  , nodeconRespE      = respE
  , nodeconExtra      = ()
  , nodeconIsUp       = shakeD
  , nodecondoLog      = doLog
  }

-- | Internal peeker to parse messages coming from peer.
peekMessage :: (MonadPeeker m, MonadIO m, MonadThrow m, PlatformNatives)
    => Network
    -> SockAddr
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
  where nodeLog = logWrite . (nodeString BTC url <>)

-- | Create version data message
mkVers :: MonadIO m => Network -> SockAddr -> m Message
mkVers net url = liftIO $ do
  ts   <- round <$> getPOSIXTime
  nonce <- randomIO
  pure $ MVersion $ Version
    { version = 70012
    , services = 0
    , timestamp = ts
    , addrRecv = NetworkAddress 0 (sockToHostAddress url)
    , addrSend = NetworkAddress 0 (sockToHostAddress $ SockAddrInet 0 0)
    , verNonce = nonce
    , userAgent = VarString (getHaskoinUserAgent net)
    , startHeight = 0
    , relay = True
    }

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
