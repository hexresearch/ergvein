module Ergvein.Index.Server.DB.Queries where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import Conversion
import Data.Default
import Data.Flat
import Data.Foldable
import Data.Maybe
import Data.Time.Clock
import Database.LevelDB
import Database.LevelDB.Iterator
import Ergvein.Index.Server.Dependencies
import Servant.Client.Core

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Text as T
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

upsertItem :: (Flat v, MonadLDB m, MonadLogger m) => BS.ByteString -> v -> m ()
upsertItem key item = do
  db <- getDb
  put db def key $ flat item



getKnownPeers :: (MonadLDB m, MonadLogger m, HasDiscoveryRequisites m) => Bool -> m [String]
getKnownPeers onlySecured = do
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
  peers <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  pure $ convert <$> peers

setKnownPeersList :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
setKnownPeersList peers = upsertItem knownPeersRecKey $ convert @_ @KnownPeerRecItem <$> peers

addKnownPeers :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = do
  let mapped = convert @_ @KnownPeerRecItem <$> peers
  stored <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  upsertItem knownPeersRecKey $ mapped ++ stored

emptyKnownPeers :: (MonadLDB m, MonadLogger m) => m ()
emptyKnownPeers = setKnownPeersList []

getScannedHeight :: (MonadLDB m, MonadLogger m) => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  stored <- getParsed $ scannedHeightTxKey currency
  pure $ scannedHeightRecHeight <$> stored

setScannedHeight :: (MonadLDB m, MonadLogger m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = upsertItem (scannedHeightTxKey currency) $ ScannedHeightRec height

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
  setScannedHeight targetCurrency (blockMetaBlockHeight $ blockInfoMeta update)

setLastScannedBlock :: (MonadLDB m, MonadLogger m) => Currency -> BlockHeaderHashHexView -> m ()
setLastScannedBlock currency blockHash = upsertItem (lastScannedBlockHeaderHashRecKey currency) blockHash

getLastScannedBlock :: (MonadLDB m, MonadLogger m) => Currency -> m (Maybe BlockHeaderHashHexView)
getLastScannedBlock currency = do
  maybeLastScannedBlock <- getParsed $ lastScannedBlockHeaderHashRecKey currency
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
      newItem = ContentHistoryRecItem  outSpendsAmountByTx newTxIds
  maybeHistory <- getParsed $ contentHistoryRecKey currency
  case maybeHistory of
    Just history | (Seq.length $ contentHistoryRecItems history) < contentHistorySize -> do
      let updatedHistory = ContentHistoryRec (contentHistoryRecItems history Seq.|> newItem)

      upsertItem (contentHistoryRecKey currency) updatedHistory
    Just history -> do
      let oldest Seq.:< restHistory = Seq.viewl $ contentHistoryRecItems history
          updatedHistory = ContentHistoryRec (restHistory Seq.|> newItem)
      
      txToUpdate <- getManyParsedExact $ txRecKey <$> (Map.keys $ contentHistoryRecItemSpentTxOuts oldest)
      write db def $ infoUpdate (contentHistoryRecItemSpentTxOuts oldest) <$> txToUpdate

      upsertItem (contentHistoryRecKey currency) updatedHistory
    Nothing -> do
      let newHistory = ContentHistoryRec $ Seq.singleton newItem
      upsertItem (contentHistoryRecKey currency) newHistory

  where
    infoUpdate spendsMap info = let 
      outputsLeft = txRecUnspentOutputsCount info - spendsMap Map.! (txRecHash info)
      in if outputsLeft == 0 then 
          LDB.Del $ txRecKey $ txRecHash info
         else
          LDB.Put (txRecKey $ txRecHash info) (flat $ info { txRecUnspentOutputsCount = outputsLeft })

revertContentHistory :: (MonadLDB m, MonadLogger m) => Currency -> m Int
revertContentHistory currency = do
  db <- getDb
  history <- getParsedExact $ contentHistoryRecKey currency
  
  let txsDeletion = LDB.Del . txRecKey <$> (contentHistoryRecItemAddedTxsHash =<< (toList $ contentHistoryRecItems history))
      newHistory = LDB.Put (contentHistoryRecKey currency) $ flat (ContentHistoryRec mempty)
      lastScannedDeletion = LDB.Del $ lastScannedBlockHeaderHashRecKey currency
      blocksRestored = Seq.length $ contentHistoryRecItems history

  write db def $ lastScannedDeletion : newHistory : txsDeletion

  pure blocksRestored