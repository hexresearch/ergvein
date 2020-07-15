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

updateTxSpends  :: (MonadLDB m, MonadLogger m) => Currency ->  BlockHeaderHashHexView -> [TxHash] -> [TxInfo] -> m ()
updateTxSpends currency blockHash spentTxsHash newTxInfos = do
  db <- getDb
  write db def $ putItems (txRecKey . txHash) (convert @_ @TxRec) newTxInfos

  let newTxIds = txHash <$> newTxInfos
      outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
      historyItem = ContentHistoryRecItem  outSpendsAmountByTx newTxIds
  history <- getParsedExact @ContentHistoryRec $ contentHistoryRecKey currency

  if (Seq.length $ contentHistoryRecItems history) < 64 then do
    let updatedHistory = ContentHistoryRec blockHash (contentHistoryRecItems history Seq.|> historyItem)
    put db def (contentHistoryRecKey currency) $ flat updatedHistory
  else do
    let oldest Seq.:< xs = Seq.viewl $ contentHistoryRecItems history
        updatedHistory = ContentHistoryRec blockHash (xs Seq.|> historyItem)
    info <- getManyParsedExact @TxRec $ txRecKey <$> (Map.keys $ contentHistoryRecItemSpentTxOuts oldest)
    write db def $ infoUpdate (contentHistoryRecItemSpentTxOuts oldest) <$> info
    put db def (contentHistoryRecKey currency) $ flat updatedHistory
  where
    infoUpdate spendsMap info = let 
      outputsLeft = txRecUnspentOutputsCount info - spendsMap Map.! (txRecHash info)
      in if outputsLeft == 0 then 
          LDB.Del $ txRecKey $ txRecHash info
         else
          LDB.Put (txRecKey $ txRecHash info) (flat $ info { txRecUnspentOutputsCount = outputsLeft })

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
  put db def knownPeersRecKey $ flat $ convert @_ @KnownPeerRecItem <$> peers

addKnownPeers :: (MonadLDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = do
  db <- getDb
  let mapped = convert @_ @KnownPeerRecItem <$> peers
  stored <- getParsedExact @[KnownPeerRecItem] knownPeersRecKey
  put db def knownPeersRecKey $ flat $ mapped ++ stored

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
  put db def (scannedHeightTxKey currency) $ flat $ ScannedHeightRec height

initDb :: DB -> IO ()
initDb db = do
  put db def knownPeersRecKey $ flat $ convert @Peer @KnownPeerRecItem <$> []

addBlockInfo :: (MonadLDB m, MonadLogger m) => BlockInfo -> m ()
addBlockInfo update = do
  db <- getDb
  updateTxSpends  (blockMetaCurrency $ blockInfoMeta update)  (blockMetaHeaderHashHexView $ blockInfoMeta update) (spentTxsHash update) (blockContentTxInfos update)
  addBlockMetaInfos [blockInfoMeta update]
  setScannedHeight (blockMetaCurrency $ blockInfoMeta update) (blockMetaBlockHeight $ blockInfoMeta update)

addBlockMetaInfos :: (MonadLDB m, MonadLogger m) => [BlockMetaInfo] -> m ()
addBlockMetaInfos infos = do
  db <- getDb
  write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = metaRecKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaRec (blockMetaHeaderHashHexView info) (blockMetaAddressFilterHexView info)
