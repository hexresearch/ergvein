module Ergvein.Index.Server.DB.Queries
  (
    -- * Indexer db queries
    getKnownPeers
  , getKnownPeersList
  , setKnownPeersList
  , addPeer
  , addKnownPeers
  , emptyKnownPeers
  , initIndexerDb
  , setLastScannedBlock
  , getLastScannedBlock
  , revertContentHistory
  -- * Filters db queries
  , getScannedHeight
  , setScannedHeight
  , addBlockMetaInfos
  -- * Combined queries
  , addBlockInfo
  , updateContentHistory
  ) where

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
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Protocol.Types

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming

getKnownPeers :: (HasIndexerDB m, MonadLogger m, HasDiscoveryRequisites m) => m [Address]
getKnownPeers = do
  idb <- getIndexerDb
  knownPeers <- getParsedExact @[KnownPeerRecItem] "getKnownPeers" idb  knownPeersRecKey
  currentTime <- liftIO getCurrentTime
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  let validDate = (-actualizationDelay) `addUTCTime` currentTime
      filteredByLastValidatedAt = filter ((validDate <=) . read . T.unpack . knownPeerRecLastValidatedAt) knownPeers 
  pure $ convert <$> filteredByLastValidatedAt

addPeer :: (HasIndexerDB m, MonadLogger m) => Peer -> m ()
addPeer peer = do
  idb <- getIndexerDb
  peerList <- getKnownPeersList
  setKnownPeersList $ peer : peerList

addKnownPeers :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = undefined

setKnownPeersList :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
setKnownPeersList peers = do
  idb <- getIndexerDb
  upsertItem idb knownPeersRecKey $ convert @_ @KnownPeerRecItem <$> peers

getKnownPeersList :: (HasIndexerDB m, MonadLogger m) => m [Peer]
getKnownPeersList = do
  idb <- getIndexerDb
  peers <- getParsedExact @[KnownPeerRecItem] "getKnownPeersList"  idb knownPeersRecKey
  pure $ convert <$> peers

emptyKnownPeers :: (HasIndexerDB m, MonadLogger m) => m ()
emptyKnownPeers = setKnownPeersList []

getScannedHeight :: (HasFiltersDB m, MonadLogger m) => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  db <- getFiltersDb
  stored <- getParsed db $ scannedHeightTxKey currency
  pure $ scannedHeightRecHeight <$> stored

setScannedHeight :: (HasFiltersDB m, MonadLogger m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getFiltersDb
  upsertItem db (scannedHeightTxKey currency) $ ScannedHeightRec height

initIndexerDb :: DB -> IO ()
initIndexerDb db = do
  write db def $ putItem knownPeersRecKey $ convert @Peer @KnownPeerRecItem <$> []

addBlockInfo :: (HasFiltersDB m, HasIndexerDB m, MonadLogger m) => BlockInfo -> m ()
addBlockInfo update = do
  db <- getFiltersDb
  let targetCurrency = blockMetaCurrency $ blockInfoMeta update
  let newBlockHash = blockMetaHeaderHashHexView $ blockInfoMeta update
  write db def $ putItems (txRecKey . txHash) (convert @_ @TxRec) (blockContentTxInfos update)
  updateContentHistory targetCurrency (spentTxsHash update) (txHash <$> blockContentTxInfos update)
  addBlockMetaInfos [blockInfoMeta update]
  setLastScannedBlock targetCurrency newBlockHash
  setScannedHeight targetCurrency (blockMetaBlockHeight $ blockInfoMeta update)

setLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> BlockHeaderHashHexView -> m ()
setLastScannedBlock currency blockHash = do
  db <- getIndexerDb
  upsertItem db (lastScannedBlockHeaderHashRecKey currency) blockHash

getLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> m (Maybe BlockHeaderHashHexView)
getLastScannedBlock currency = do
  db <- getIndexerDb
  maybeLastScannedBlock <- getParsed db $ lastScannedBlockHeaderHashRecKey currency
  pure $ lastScannedBlockHeaderHashRecHash <$> maybeLastScannedBlock

addBlockMetaInfos :: (HasFiltersDB m, MonadLogger m) => [BlockMetaInfo] -> m ()
addBlockMetaInfos infos = do
  db <- getFiltersDb
  write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = metaRecKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaRec (blockMetaHeaderHashHexView info) (blockMetaAddressFilterHexView info)

updateContentHistory  :: (HasFiltersDB m, HasIndexerDB m, MonadLogger m) => Currency -> [TxHash] -> [TxHash] -> m ()
updateContentHistory currency spentTxsHash newTxIds = do
  idb <- getIndexerDb
  fdb <- getFiltersDb
  let outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
      newItem = ContentHistoryRecItem  outSpendsAmountByTx newTxIds
  maybeHistory <- getParsed idb $ contentHistoryRecKey currency
  case maybeHistory of
    Just history | (Seq.length $ contentHistoryRecItems history) < contentHistorySize -> do
      let updatedHistory = ContentHistoryRec (contentHistoryRecItems history Seq.|> newItem)

      upsertItem idb (contentHistoryRecKey currency) updatedHistory
    Just history -> do
      let oldest Seq.:< restHistory = Seq.viewl $ contentHistoryRecItems history
          updatedHistory = ContentHistoryRec (restHistory Seq.|> newItem)

      txToUpdate <- getManyParsedExact "updateContentHistory" fdb $ txRecKey <$> (Map.keys $ contentHistoryRecItemSpentTxOuts oldest)
      write idb def $ infoUpdate (contentHistoryRecItemSpentTxOuts oldest) <$> txToUpdate

      upsertItem idb (contentHistoryRecKey currency) updatedHistory
    Nothing -> do
      let newHistory = ContentHistoryRec $ Seq.singleton newItem
      upsertItem idb (contentHistoryRecKey currency) newHistory

  where
    infoUpdate spendsMap info = let
      outputsLeft = txRecUnspentOutputsCount info - spendsMap Map.! (txRecHash info)
      in if outputsLeft == 0 then
          LDB.Del $ txRecKey $ txRecHash info
         else
          LDB.Put (txRecKey $ txRecHash info) (flat $ info { txRecUnspentOutputsCount = outputsLeft })

revertContentHistory :: (HasIndexerDB m, MonadLogger m) => Currency -> m Int
revertContentHistory currency = do
  db <- getIndexerDb
  history <- getParsedExact "revertContentHistory" db $ contentHistoryRecKey currency

  let txsDeletion = LDB.Del . txRecKey <$> (contentHistoryRecItemAddedTxsHash =<< (toList $ contentHistoryRecItems history))
      newHistory = LDB.Put (contentHistoryRecKey currency) $ flat (ContentHistoryRec mempty)
      lastScannedDeletion = LDB.Del $ lastScannedBlockHeaderHashRecKey currency
      blocksRestored = Seq.length $ contentHistoryRecItems history

  write db def $ lastScannedDeletion : newHistory : txsDeletion

  pure blocksRestored
