{-# OPTIONS_GHC -Wno-all #-}
module Ergvein.Wallet.Indexer.Socket
  (
    initIndexerConnection
  , IndexerConnection(..)
  , IndexerMsg(..)
  , IndexReqSelector
  ) where

import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Maybe
import Data.Text
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word
import Network.Socket hiding (socket)
import Reflex
import UnliftIO hiding (atomically)

import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Text
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.Socket
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as M
import qualified Data.Vector.Unboxed        as VU

initIndexerConnection :: (MonadBaseConstr t m, MonadHasSettings t m) => NamedSockAddr -> Event t IndexerMsg ->  m (IndexerConnection t)
initIndexerConnection (NamedSockAddr sname sa) msgE = mdo
  (versionMismatchE, versionMismatchFire) <- newTriggerEvent
  versionMismatchDE <- delay 0.2 versionMismatchE
  (msname, msport) <- liftIO $ getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True sa
  let peer = fromJust $ Peer <$> msname <*> msport
  let restartE = fforMaybe msgE $ \case
        IndexerRestart -> Just ()
        _ -> Nothing
      closeE = fforMaybe msgE $ \case
        IndexerClose -> Just ()
        _ -> Nothing
      reqE = fforMaybe msgE $ \case
        IndexerMsg req -> Just req
        _ -> Nothing
  proxyD <- getSocksConf
  s <- socket SocketConf {
      _socketConfPeer   = peer
    , _socketConfSend   = fmap serializeMessage sendE
    , _socketConfPeeker = peekMessage sa
    , _socketConfClose  = leftmost [closeE, versionMismatchDE]
    , _socketConfReopen = Just (1, 2) -- reconnect after 1 seconds 2 retries
    , _socketConfProxy  = proxyD
    }
  handshakeE <- performEvent $ ffor (socketConnected s) $ const $ mkVers
  let respE = _socketInbound s
  hsRespE <- fmap (fmapMaybe id) $ performFork $ ffor respE $ \case
    MReject (Reject VersionNotSupported) -> do
      nodeLog sa $ "The remote version is not compatible with our version " <> showt protocolVersion
      liftIO $ versionMismatchFire ()
      pure Nothing
    MVersion Version{..} -> do
      nodeLog sa $ "Received version: " <> showt versionVersion
      if protocolVersion `isCompatible` versionVersion
        then pure $ Just (MVersionACK VersionACK)
        else do
          nodeLog sa $ "The reported remote version " <> showt versionVersion <> " is not compatible with our version " <> showt protocolVersion
          liftIO $ versionMismatchFire ()
          pure Nothing
    MPing nonce -> pure $ Just $ MPong nonce
    _ -> pure Nothing
  let sendE = leftmost [handshakeE, hsRespE, gate (current shakeD) reqE]

  performEvent_ $ ffor (_socketRecvEr s) $ nodeLog sa . showt

  -- Track handshake status
  let verAckE = fforMaybe respE $ \case
        MVersionACK _ -> Just True
        _ -> Nothing
  shakeD <- holdDyn False $ leftmost [verAckE, False <$ closeE]
  let openE = fmapMaybe (\b -> if b then Just () else Nothing) $ updated shakeD

  -- Track filters height

  let setHE = fforMaybe respE $ \case
        MVersion Version{..} -> Just $ M.fromList $ ffor (VU.toList versionScanBlocks) $
          \ScanBlock{..} -> (currencyCodeToCurrency scanBlockCurrency, scanBlockScanHeight)
        MFiltersEvent FilterEvent{..} -> let k = currencyCodeToCurrency filterEventCurrency
          in Just $ M.singleton k filterEventHeight
        _ -> Nothing
  heightsD <- foldDyn M.union M.empty setHE

  pure $ IndexerConnection {
      indexConAddr = sa
    , indexConName = sname
    , indexConClosedE = () <$ _socketClosed s
    , indexConOpensE = openE
    , indexConIsUp = shakeD
    , indexConRespE = respE
    , indexConHeight = heightsD
    , indexConStatus = pure IndexerOk
    }
  where
    serializeMessage :: Message -> B.ByteString
    serializeMessage = BL.toStrict . BB.toLazyByteString . messageBuilder

-- | Internal peeker to parse messages coming from peer.
peekMessage :: (MonadPeeker m, MonadIO m, MonadThrow m, PlatformNatives)
  => SockAddr -> m Message
peekMessage url = do
  x <- peek 8
  let ehead = AP.eitherResult $ AP.parse messageHeaderParser x
  case ehead of
    Left e -> do
      nodeLog url $ "Consumed " <> showt (B.length x)
      nodeLog url $ "Could not decode incoming message header: " <> showt e
      nodeLog url $ showt x
      throwM DecodeHeaderError
    Right (MessageHeader !msgType !len) -> do
      -- nodeLog $ showt cmd
      when (len > 32 * 2 ^ (20 :: Int)) $ do
        nodeLog url "Payload too large"
        throwM (PayloadTooLarge len)
      y <- peek (fromIntegral len)
      let emsg = AP.parseOnly (messageParser msgType) y
      case emsg of
        Left e -> do
          nodeLog url $ "Cannot decode payload: " <> showt e
          throwM CannotDecodePayload
        Right !msg -> pure msg

-- | Create version data message
mkVers :: MonadIO m => m Message
mkVers = liftIO $ do
  nonce <- randomIO
  t <- fmap (fromIntegral . floor) getPOSIXTime
  pure $ MVersion $ Version {
      versionVersion    = protocolVersion
    , versionTime       = t
    , versionNonce      = nonce
    , versionScanBlocks = mempty
    }

-- | Node string for logging
nodeLog :: (PlatformNatives, MonadIO m) => SockAddr -> Text -> m ()
nodeLog url txt = logWrite $ "[Indexer]<" <> showt url <> ">: " <> txt

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
