{-# LANGUAGE MultiWayIf #-}
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
import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Ergvein.Text
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Sepulcas.Native
import Ergvein.Wallet.Node.Socket
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.Map.Strict            as M
import qualified Data.Vector.Unboxed        as VU

requiredCurrencies :: [CurrencyCode]
requiredCurrencies = if isTestnet
  then [TBTC] -- TODO: add ERGO here
  else [BTC]

hasRequiredCurrs :: Version -> Bool
hasRequiredCurrs = (`versionHasCurrs` requiredCurrencies)

requiredCurrsSynced :: Version -> Bool
requiredCurrsSynced = (`versionCurrsSynced` requiredCurrencies)

initIndexerConnection :: (MonadBaseConstr t m, MonadHasSettings t m) => ErgveinNodeAddr -> SockAddr -> Event t IndexerMsg ->  m (IndexerConnection t)
initIndexerConnection sname sa msgE = mdo
  (versionMismatchE, versionMismatchFire) <- newTriggerEvent
  (currenciesMismatchE, currenciesMismatchFire) <- newTriggerEvent
  (currenciesNotSyncedE, currenciesNotSyncedFire) <- newTriggerEvent
  (versionE, versionFire) <- newTriggerEvent
  versionMismatchDE <- delay 0.2 $ void versionMismatchE
  currenciesMismatchDE <- delay 0.2 currenciesMismatchE
  currenciesNotSyncedDE <- delay 0.2 currenciesNotSyncedE
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
    , _socketConfClose  = leftmost [closeE, versionMismatchDE, currenciesMismatchDE, currenciesNotSyncedDE]
    , _socketConfReopen = Just (1, 2) -- reconnect after 1 seconds 2 retries
    , _socketConfProxy  = proxyD
    }
  handshakeE <- performEvent $ ffor (socketConnected s) $ const $ mkVers
  let respE = _socketInbound s
  hsRespE <- fmap (fmapMaybe id) $ performFork $ ffor respE $ \case
    MReject (Reject _ VersionNotSupported _) -> do
      nodeLog sa $ "The remote version is not compatible with our version " <> showt protocolVersion
      liftIO $ versionMismatchFire Nothing
      pure Nothing
    MReject (Reject i c msg) -> do
      nodeLog sa $ "The remote server rejected for msg " <> showt i <> " with code " <> showt c <> " and message: " <> msg
      pure Nothing
    MVersion v@Version{..} -> do
      nodeLog sa $ "Received version: " <> showt versionVersion
      if | not $ protocolVersion `isCompatible` versionVersion -> do
            nodeLog sa $ "The reported remote version " <> showt versionVersion <> " is not compatible with our version " <> showt protocolVersion
            liftIO $ versionMismatchFire (Just versionVersion)
            pure Nothing
         | not $ hasRequiredCurrs v -> do
            nodeLog sa $ "The indexer doesn't support required currencies " <> showt requiredCurrencies <> ", but got: " <> showt (versionCurrencies v)
            liftIO $ currenciesMismatchFire ()
            pure Nothing
         | not $ requiredCurrsSynced v -> do
           nodeLog sa $ "The indexer is not fully synced for currencies " <> showt requiredCurrencies
           liftIO $ currenciesNotSyncedFire ()
           pure Nothing
         | otherwise -> do
            liftIO $ versionFire versionVersion
            pure $ Just (MVersionACK VersionACK)
    MPing nonce -> pure $ Just $ MPong nonce
    _ -> pure Nothing
  let sendE = leftmost [handshakeE, hsRespE, gate (current shakeD) reqE]

  performEvent_ $ ffor (_socketRecvEr s) $ nodeLog sa . showt

  -- Track handshake status
  let verAckE = fforMaybe respE $ \case
        MVersionACK _ -> Just True
        _ -> Nothing
  versionD <- holdDyn Nothing $ Just <$> versionE
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

  statusD <- holdDyn IndexerOk $ leftmost [
      IndexerWrongVersion <$> versionMismatchE
    , IndexerMissingCurrencies <$ currenciesMismatchE
    , IndexerNotSynced <$ currenciesNotSyncedE ]
  pure $ IndexerConnection {
      indexConAddr = sa
    , indexConName = sname
    , indexConIndexerVersion = versionD
    , indexConClosedE = () <$ _socketClosed s
    , indexConOpensE = openE
    , indexConIsUp = shakeD
    , indexConRespE = respE
    , indexConHeight = heightsD
    , indexConStatus = statusD
    }
  where
    serializeMessage :: Message -> B.ByteString
    serializeMessage = BL.toStrict . BB.toLazyByteString . messageBuilder

-- | Internal peeker to parse messages coming from peer.
peekMessage :: (MonadPeeker m, MonadIO m, MonadThrow m, PlatformNatives)
  => SockAddr -> m Message
peekMessage url = do
  MessageHeader !msgType !len <- peekHeader url
  nodeLog url $ showt (MessageHeader msgType len)
  when (len > 32 * 2 ^ (20 :: Int)) $ do
    nodeLog url "Payload too large"
    throwM (PayloadTooLarge len)
  y <- if len == 0 then pure mempty else  peek (fromIntegral len)
  let emsg = AP.parseOnly (messageParser msgType) y
  case emsg of
    Left e -> do
      nodeLog url $ "Cannot decode payload: " <> showt e
      throwM CannotDecodePayload
    Right !msg -> pure msg

peekVarInt :: MonadPeeker m => m B.ByteString
peekVarInt = do
  hbs <- peek 1
  if B.null hbs then pure hbs else do
    let hb = B.head hbs
    if | hb == 0xFF -> (hbs <>) <$> peek 8
       | hb == 0XFE -> (hbs <>) <$> peek 4
       | hb == 0xFD -> (hbs <>) <$> peek 2
       | otherwise -> pure hbs

peekHeader :: (MonadPeeker m, MonadIO m, MonadThrow m, PlatformNatives) => SockAddr -> m MessageHeader
peekHeader url = do
  mid <- parseType =<< peekVarInt
  if not $ messageHasPayload mid then pure (MessageHeader mid 0) else do
    l <- parseLength =<< peekVarInt
    pure $ MessageHeader mid l
  where
    parseType bs = case AP.eitherResult . AP.parse messageTypeParser $ bs of
      Left e -> do
        nodeLog url $ "Could not decode incoming message header, type id: " <> showt e
        throwM DecodeHeaderError
      Right a -> pure a
    parseLength bs = case AP.eitherResult . AP.parse messageLengthParser $ bs of
      Left e -> do
        nodeLog url $ "Could not decode incoming message header, message length: " <> showt e
        throwM DecodeHeaderError
      Right a -> pure a

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
