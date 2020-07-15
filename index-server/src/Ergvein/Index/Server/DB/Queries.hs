module Ergvein.Index.Server.DB.Queries where

import Control.Lens
import Data.Default
import Data.Flat
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Iterator
import Control.Monad.Logger
import Conversion
import Data.Foldable
import Data.Time.Clock
import Control.Monad.IO.Class
import Ergvein.Index.Server.Dependencies
import Servant.Client.Core

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
import qualified Data.Sequence as Seq

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

getManyParsedExact :: (Flat v, MonadLDB m, MonadLogger m) => [BS.ByteString] -> m [v]
getManyParsedExact keys = do
  db <- getDb
  result <- mapM getParsedExact keys
  pure result

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

putItem :: (Flat v) => BS.ByteString -> v -> LDB.WriteBatch
putItem key item = [LDB.Put key $ flat item]  

revertContentHistory :: (MonadLDB m, MonadLogger m, HasDiscoveryRequisites m) => Currency -> m Int
revertContentHistory currency = do
  db <- getDb
  history <- getParsedExact @ContentHistoryRec $ contentHistoryRecKey currency
  
  let txsDeletion = LDB.Del . txRecKey <$> (contentHistoryRecItemAddedTxsHash =<< (toList $ contentHistoryRecItems history))
      historyDeletion = LDB.Del $ contentHistoryRecKey currency
      blocksRestored = Seq.length $ contentHistoryRecItems history

  write db def $ historyDeletion : txsDeletion
  pure blocksRestored

getKnownPeers :: (MonadLDB m, MonadLogger m, HasDiscoveryRequisites m) => Bool -> m [String]
getKnownPeers onlySecured = do
  db <- getDb
  knownPeers <- fmap convert <$> getParsedExact @[KnownPeerRecItem]  knownPeersRecKey
  currentTime <- liftIO getCurrentTime
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  let validDate = (-actualizationDelay) `addUTCTime` currentTime
      filteredByOnlySecured = 
        if onlySecured then
          filter (\p -> 
            case peerConnScheme p of
              Https -> True
              _     -> False)
          knownPeers 
        else 
          knownPeers
      filteredByLastValidatedAt = filter ((validDate <=) . peerLastValidatedAt) filteredByOnlySecured 
  pure $ showBaseUrl . peerUrl <$> filteredByLastValidatedAt

getKnownPeersList :: (MonadLDB m, MonadLogger m) => m [Peer]
getKnownPeersList = do
  db <- getDb
  peers <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  pure $ convert <$> peers

setKnownPeersList :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
setKnownPeersList peers = do
  db <- getDb
  write db def $ putItem knownPeersRecKey $ convert @_ @KnownPeerRecItem <$> peers

addKnownPeers :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = do
  db <- getDb
  let mapped = convert @_ @KnownPeerRecItem <$> peers
  stored <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  write db def $ putItem knownPeersRecKey $ mapped ++ stored

emptyKnownPeers :: (MonadLDB m, MonadLogger m) => m ()
emptyKnownPeers = setKnownPeersList []

getScannedHeight :: (MonadLDB m, MonadLogger m) => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  db <- getDb
  stored <- getParsed $ scannedHeightTxKey currency
  pure $ scannedHeightRecHeight <$> stored

setScannedHeight :: (MonadLDB m, MonadLogger m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getDb
  write db def $ putItem (scannedHeightTxKey currency) $ ScannedHeightRec height

initDb :: DB -> IO ()
initDb db = do
  write db def $ putItem knownPeersRecKey $ convert @Peer @KnownPeerRecItem <$> []

addBlockInfo :: (MonadLDB m, MonadLogger m) => BlockInfo -> m ()
addBlockInfo update = do
  db <- getDb
  let targetCurrency = blockMetaCurrency $ blockInfoMeta update
  let newBlockHash = blockMetaHeaderHashHexView $ blockInfoMeta update
  write db def $ putItems (txRecKey . txHash) (convert @_ @TxRec) (blockContentTxInfos update)
  updateContentHistory targetCurrency (spentTxsHash update) (txHash <$> blockContentTxInfos update)
  addBlockMetaInfos [blockInfoMeta update]
  setLastScannedBlock targetCurrency newBlockHash
  setScannedHeight (blockMetaCurrency $ blockInfoMeta update) (blockMetaBlockHeight $ blockInfoMeta update)

setLastScannedBlock :: (MonadLDB m, MonadLogger m) => Currency -> BlockHeaderHashHexView -> m ()
setLastScannedBlock currency blockHash = do
  db <- getDb
  write db def $ putItem (lastScannedBlockHeaderHashRecKey currency) blockHash

getLastScannedBlock :: (MonadLDB m, MonadLogger m) => Currency -> m (Maybe BlockHeaderHashHexView)
getLastScannedBlock currency = do
  db <- getDb
  maybeLastScannedBlock <- getParsed @LastScannedBlockHeaderHashRec (lastScannedBlockHeaderHashRecKey currency)
  pure $ lastScannedBlockHeaderHashRecHash <$> maybeLastScannedBlock

addBlockMetaInfos :: (MonadLDB m, MonadLogger m) => [BlockMetaInfo] -> m ()
addBlockMetaInfos infos = do
  db <- getDb
  write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = metaRecKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaRec (blockMetaHeaderHashHexView info) (blockMetaAddressFilterHexView info)

updateContentHistory  :: (MonadLDB m, MonadLogger m) => Currency -> [TxHash] -> [TxHash] -> m ()
updateContentHistory currency spentTxsHash newTxIds = do
  db <- getDb
  let outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
      historyItem = ContentHistoryRecItem  outSpendsAmountByTx newTxIds
  maybeHistory <- getParsed @ContentHistoryRec $ contentHistoryRecKey currency
  case maybeHistory of
    Just history | (Seq.length $ contentHistoryRecItems history) < contentHistorySize -> do
      let updatedHistory = ContentHistoryRec (contentHistoryRecItems history Seq.|> historyItem)
      write db def $ putItem (contentHistoryRecKey currency) updatedHistory
    Just history -> do
      let oldest Seq.:< xs = Seq.viewl $ contentHistoryRecItems history
          updatedHistory = ContentHistoryRec (xs Seq.|> historyItem)
      info <- getManyParsedExact @TxRec $ txRecKey <$> (Map.keys $ contentHistoryRecItemSpentTxOuts oldest)
      write db def $ infoUpdate (contentHistoryRecItemSpentTxOuts oldest) <$> info
      write db def $ putItem (contentHistoryRecKey currency) updatedHistory
    Nothing -> do
      let emptyHistory = ContentHistoryRec mempty
      write db def $ putItem (contentHistoryRecKey currency) emptyHistory

  where
    infoUpdate spendsMap info = let 
      outputsLeft = txRecUnspentOutputsCount info - spendsMap Map.! (txRecHash info)
      in if outputsLeft == 0 then 
          LDB.Del $ txRecKey $ txRecHash info
         else
          LDB.Put (txRecKey $ txRecHash info) (flat $ info { txRecUnspentOutputsCount = outputsLeft })