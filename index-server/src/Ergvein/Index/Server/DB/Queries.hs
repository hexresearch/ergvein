module Ergvein.Index.Server.DB.Queries
  (
    -- * Indexer db queries
    getActualPeers
  , getPeerList
  , setPeerList
  , upsertPeer
  , emptyKnownPeers
  , deletePeerBySockAddr
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

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Conversion
import Data.ByteString.Short (ShortByteString)
import Data.Default
import Data.Foldable
import Data.Time.Clock
import Database.LevelDB
import Database.LevelDB.Iterator
import Network.Socket

import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions()
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Protocol.Types
import Ergvein.Types.Currency as Currency
import Ergvein.Index.Server.DB.Serialize

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Database.LevelDB as LDB
import qualified Data.Text as T

getActualPeers :: (HasIndexerDB m, MonadLogger m, HasDiscoveryRequisites m) =>  m [Address]
getActualPeers = do
  db <- getIndexerDb
  -- I put BTC here and downstream, because it doesnt actually matter but we still need a value
  knownPeers <- getParsedExact @KnownPeersRec Currency.BTC "getKnownPeers" db knownPeersRecKey
  currentTime <- liftIO getCurrentTime
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  let validDate = (-actualizationDelay) `addUTCTime` currentTime
      filteredByLastValidatedAt = filter ((validDate <=) . read . T.unpack . knownPeerRecLastValidatedAt) $ unKnownPeersRec knownPeers
  pure $ convert <$> filteredByLastValidatedAt

getKnownPeersList :: (HasIndexerDB m, MonadLogger m) => m [Peer]
getKnownPeersList = do
  db <- getIndexerDb
  peers <- getParsedExact @KnownPeersRec Currency.BTC "getKnownPeersList" db knownPeersRecKey
  pure $ convert <$> (unKnownPeersRec peers)

setKnownPeersList :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
setKnownPeersList peers = do
  db <- getIndexerDb
  upsertItem Currency.BTC db knownPeersRecKey $ KnownPeersRec $ convert @_ @KnownPeerRecItem <$> peers

addKnownPeers :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = do
  db <- getIndexerDb
  let mapped = convert @_ @KnownPeerRecItem <$> peers
  stored <- getParsedExact @KnownPeersRec Currency.BTC "addKnownPeers" db knownPeersRecKey
  upsertItem Currency.BTC db knownPeersRecKey $ KnownPeersRec $ mapped ++ (unKnownPeersRec stored)


getPeerList :: (HasIndexerDB m, MonadLogger m) => m [Peer]
getPeerList = do
  idb <- getIndexerDb
  peers <- getParsedExact  @KnownPeersRec Currency.BTC "getKnownPeersList"  idb knownPeersRecKey
  pure $ convert <$> unKnownPeersRec peers

setPeerList :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
setPeerList peers = do
  idb <- getIndexerDb
  upsertItem Currency.BTC  idb knownPeersRecKey $ KnownPeersRec $ convert @_ @KnownPeerRecItem <$> peers

upsertPeer :: (HasIndexerDB m, MonadLogger m) => Peer -> m ()
upsertPeer peer = do
  currentList <- peerList
  let peerRec = convert peer
  setPeerRecList $ KnownPeersRec $ peerRec : excludePeerByAddr (knownPeerRecAddr peerRec) (unKnownPeersRec currentList)

peerList :: (HasIndexerDB m, MonadLogger m) => m KnownPeersRec
peerList = do
  idb <- getIndexerDb
  getParsedExact @KnownPeersRec Currency.BTC "getKnownPeersList"  idb knownPeersRecKey

setPeerRecList :: (HasIndexerDB m, MonadLogger m) => KnownPeersRec -> m ()
setPeerRecList peers = do
  idb <- getIndexerDb
  upsertItem Currency.BTC idb knownPeersRecKey peers

deletePeerBySockAddr :: (HasIndexerDB m, MonadLogger m) => PeerAddr -> m ()
deletePeerBySockAddr addr = do
  currentList <- peerList
  setPeerRecList $ KnownPeersRec $ excludePeerByAddr addr  $ unKnownPeersRec currentList

excludePeerByAddr :: PeerAddr ->  [KnownPeerRecItem] -> [KnownPeerRecItem]
excludePeerByAddr addr = filter ((== addr) . knownPeerRecAddr)

emptyKnownPeers :: (HasIndexerDB m, MonadLogger m) => m ()
emptyKnownPeers = setPeerRecList $ KnownPeersRec []

getScannedHeight :: (HasFiltersDB m, MonadLogger m) => Currency -> m (Maybe BlockHeight)
getScannedHeight currency = do
  db <- getFiltersDb
  stored <- getParsed currency "BlockHeight" db $ scannedHeightTxKey currency
  pure $ scannedHeightRecHeight <$> stored

setScannedHeight :: (HasFiltersDB m, MonadLogger m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getFiltersDb
  upsertItem currency db (scannedHeightTxKey currency) $ ScannedHeightRec height

initIndexerDb :: DB -> IO ()
initIndexerDb db = do
  write db def $ putItem Currency.BTC knownPeersRecKey $ KnownPeersRec []

addBlockInfo :: (HasFiltersDB m, HasIndexerDB m, MonadLogger m)
  => BlockInfo -> BlockHeight -> m ()
addBlockInfo update to = do
  db <- getFiltersDb
  let current = blockMetaBlockHeight $ blockInfoMeta update
  let targetCurrency = blockMetaCurrency $ blockInfoMeta update
  let newBlockHash = blockMetaHeaderHash $ blockInfoMeta update
  write db def $ putTxInfosAsRecs targetCurrency (blockContentTxInfos update)
  when (to - current <= 64) $
    updateContentHistory targetCurrency (spentTxsHash update) (txHash <$> blockContentTxInfos update)
  addBlockMetaInfos targetCurrency [blockInfoMeta update]
  setLastScannedBlock targetCurrency newBlockHash
  setScannedHeight targetCurrency (blockMetaBlockHeight $ blockInfoMeta update)

setLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> ShortByteString -> m ()
setLastScannedBlock currency blockHash = do
  db <- getIndexerDb
  upsertItem currency db (lastScannedBlockHeaderHashRecKey currency) $ LastScannedBlockHeaderHashRec blockHash

getLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> m (Maybe ShortByteString)
getLastScannedBlock currency = do
  db <- getIndexerDb
  maybeLastScannedBlock <- getParsed currency "lastScannedBlockHeaderHashRecKey" db $ lastScannedBlockHeaderHashRecKey currency
  pure $ lastScannedBlockHeaderHashRecHash <$> maybeLastScannedBlock

-- Currency should be consistent with currency in BlockInfoMeta
addBlockMetaInfos :: (HasFiltersDB m, MonadLogger m) => Currency -> [BlockMetaInfo] -> m ()
addBlockMetaInfos currency infos = do
  db <- getFiltersDb
  write db def $ putItems currency keySelector valueSelector infos
  where
    keySelector   info = metaRecKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaRec (blockMetaHeaderHash info) (blockMetaAddressFilter info)

updateContentHistory  :: (HasFiltersDB m, HasIndexerDB m, MonadLogger m) => Currency -> [TxHash] -> [TxHash] -> m ()
updateContentHistory currency spentTxsHash newTxIds = do
  idb <- getIndexerDb
  fdb <- getFiltersDb
  let outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
      newItem = ContentHistoryRecItem  outSpendsAmountByTx newTxIds
  maybeHistory <- getParsed currency "contentHistoryRecKey" idb $ contentHistoryRecKey currency
  case maybeHistory of
    Just history | (Seq.length $ contentHistoryRecItems history) < contentHistorySize -> do
      let updatedHistory = ContentHistoryRec (contentHistoryRecItems history Seq.|> newItem)

      upsertItem currency idb (contentHistoryRecKey currency) updatedHistory
    Just history -> do
      let oldest Seq.:< restHistory = Seq.viewl $ contentHistoryRecItems history
          updatedHistory = ContentHistoryRec (restHistory Seq.|> newItem)

      txToUpdate <- getManyParsedExact currency "updateContentHistory" fdb $ txRecKey <$> (Map.keys $ contentHistoryRecItemSpentTxOuts oldest)
      write idb def $ infoUpdate (contentHistoryRecItemSpentTxOuts oldest) <$> txToUpdate

      upsertItem currency idb (contentHistoryRecKey currency) updatedHistory
    Nothing -> do
      let newHistory = ContentHistoryRec $ Seq.singleton newItem
      upsertItem currency idb (contentHistoryRecKey currency) newHistory

  where
    infoUpdate spendsMap info = let
      outputsLeft = txRecUnspentOutputsCount info - spendsMap Map.! (txRecHash info)
      in if outputsLeft == 0 then
          LDB.Del $ txRecKey $ txRecHash info
         else
          LDB.Put (txRecKey $ txRecHash info) (egvSerialize currency $ info { txRecUnspentOutputsCount = outputsLeft })

revertContentHistory :: (HasIndexerDB m, MonadLogger m) => Currency -> m Int
revertContentHistory currency = do
  db <- getIndexerDb
  history <- getParsedExact currency "revertContentHistory" db $ contentHistoryRecKey currency

  let txsDeletion = LDB.Del . txRecKey <$> (contentHistoryRecItemAddedTxsHash =<< (toList $ contentHistoryRecItems history))
      newHistory = LDB.Put (contentHistoryRecKey currency) $ egvSerialize currency (ContentHistoryRec mempty)
      lastScannedDeletion = LDB.Del $ lastScannedBlockHeaderHashRecKey currency
      blocksRestored = Seq.length $ contentHistoryRecItems history

  write db def $ lastScannedDeletion : newHistory : txsDeletion

  pure blocksRestored
