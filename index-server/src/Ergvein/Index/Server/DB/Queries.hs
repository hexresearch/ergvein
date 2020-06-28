module Ergvein.Index.Server.DB.Queries where

import Control.Lens
import Data.Default
import Data.Flat
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Iterator
import Control.Monad.Logger
import Conversion
import Data.Time.Clock
import Control.Monad.IO.Class
import Ergvein.Index.Server.Dependencies

import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Types.Transaction
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming

safeEntrySlice :: (MonadLDB m , Ord k, S.Serialize k, Flat v) => BS.ByteString -> k -> m [(k,v)]
safeEntrySlice startKey endKey = do
  db <- getDb
  iterator <- createIter db def
  slice <- LDBStreaming.toList $ LDBStreaming.entrySlice iterator range LDBStreaming.Asc
  pure $ over _2 unflatExact  <$> over _1 (decodeExact . unPrefixedKey) <$> slice
  where
    range = LDBStreaming.KeyRange startKey comparison
    comparison key = case S.decode $ unPrefixedKey key of
      Right parsedKey -> compare parsedKey endKey
      _ -> GT

getParsed :: (Flat v, MonadLDB m) => BS.ByteString -> m (Maybe v)
getParsed key = do
  db <- getDb
  maybeResult <- get db def key
  let maybeParsedResult = unflatExact <$> maybeResult
  pure maybeParsedResult

getParsedExact :: (Flat v, MonadLDB m, MonadLogger m) => BS.ByteString -> m v
getParsedExact key = do
  db <- getDb
  maybeResult <- get db def key
  case maybeResult of
    Just result -> pure $ unflatExact result
    Nothing -> do
      currentTime <- liftIO getCurrentTime
      logErrorN $ "[Db read miss][getParsedExact] Entity with key " <> (T.pack $ show key) <> " not found at time:" <> (T.pack $ show currentTime)
      error $ "getParsedExact: not found" ++ show key

getManyParsedExact :: (MonadLDB m, MonadLogger m, Flat v) => [BS.ByteString] -> m [v]
getManyParsedExact keys = do
  db <- getDb
  result <- mapM getParsedExact keys
  pure result

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

updateTxSpends  :: (MonadLDB m, MonadLogger m) => [TxHash] -> [TxInfo] -> m ()
updateTxSpends spentTxsHash newTxInfos = do
  db <- getDb
  write db def $ putItems (txRecKey . txHash) (convert @_ @TxRec) newTxInfos
  info <- getManyParsedExact @_ @TxRec  $ txRecKey <$> Map.keys outSpendsAmountByTx 
  write db def $ infoUpdate <$> info
  where
    outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
    infoUpdate info = let 
      outputsLeft = txRecUnspentOutputsCount info - outSpendsAmountByTx Map.! (txRecHash info)
      in if outputsLeft == 0 then 
          LDB.Del $ txRecKey $ txRecHash info
         else
          LDB.Put (txRecKey $ txRecHash info) (flat $ info { txRecUnspentOutputsCount = outputsLeft })

getKnownPeers :: (MonadLDB m, MonadLogger m, HasDiscoveryRequisites m) => Bool -> m [String]
getKnownPeers onlySecured = do
  db <- getDb
  knownPeers <- getParsedExact knownPeersRecKey
  currentTime <- liftIO getCurrentTime
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  let validDate = (-actualizationDelay) `addUTCTime` currentTime
  pure $ T.unpack . knownPeerRecUrl <$>
    if onlySecured then filter knownPeerRecIsSecureConn knownPeers else knownPeers

getKnownPeersList :: (MonadLDB m, MonadLogger m) => m [Peer]
getKnownPeersList = do
  db <- getDb
  peers <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  pure $ convert <$> peers

setKnownPeersList :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
setKnownPeersList peers = do
  db <- getDb
  put db def knownPeersRecKey $ flat $ convert @_ @KnownPeerRecItem <$> peers

addKnownPeers :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = do
  db <- getDb
  let mapped = convert @_ @KnownPeerRecItem <$> peers
  stored <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  put db def knownPeersRecKey $ flat $ mapped ++ stored

getScannedHeight :: (MonadLDB m, MonadLogger m) => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  db <- getDb
  stored <- getParsed $ scannedHeightTxKey currency
  pure $ scannedHeightRecHeight <$> stored

setScannedHeight :: (MonadLDB m, MonadLogger m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getDb
  put db def (scannedHeightTxKey currency) $ flat $ ScannedHeightRec height

initDb :: DB -> IO ()
initDb db = do
  put db def knownPeersRecKey $ flat $ convert @Peer @KnownPeerRecItem <$> []

addBlockInfo :: (MonadLDB m, MonadLogger m) => BlockInfo -> m ()
addBlockInfo update = do
  db <- getDb
  updateTxSpends (spentTxsHash update) $ blockContentTxInfos update
  addBlockMetaInfos [blockInfoMeta update]
  setScannedHeight (blockMetaCurrency $ blockInfoMeta update) (blockMetaBlockHeight $ blockInfoMeta update)

addBlockMetaInfos :: (MonadLDB m, MonadLogger m) => [BlockMetaInfo] -> m ()
addBlockMetaInfos infos = do
  db <- getDb
  write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = metaRecKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaRec (blockMetaHeaderHashHexView info) (blockMetaAddressFilterHexView info)