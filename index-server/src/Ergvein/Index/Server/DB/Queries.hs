module Ergvein.Index.Server.DB.Queries
  (
    -- * Indexer db queries
    getKnownPeers
  , getKnownPeersList
  , setKnownPeersList
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

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Conversion
import Data.ByteString.Short (ShortByteString)
import Data.Default
import Data.Foldable
import Data.Time.Clock
import Database.LevelDB
import Ergvein.Index.Server.Dependencies
import Servant.Client.Core

import Ergvein.Index.Server.DB.Serialize(EgvSerialize(..), putTxInfosAsRecs)
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Conversions()
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Database.LevelDB as LDB

getKnownPeers :: (HasIndexerDB m, MonadLogger m, HasDiscoveryRequisites m) => Bool -> m [String]
getKnownPeers onlySecured = do
  db <- getIndexerDb
  -- I put BTC here and downstream, because it doesnt actually matter but we still need a value
  knownPeers <- fmap (fmap convert . unKnownPeersRec) $ getParsedExact @KnownPeersRec BTC "getKnownPeers" db knownPeersRecKey
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

getKnownPeersList :: (HasIndexerDB m, MonadLogger m) => m [Peer]
getKnownPeersList = do
  db <- getIndexerDb
  peers <- getParsedExact @KnownPeersRec BTC "getKnownPeersList" db knownPeersRecKey
  pure $ convert <$> (unKnownPeersRec peers)

setKnownPeersList :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
setKnownPeersList peers = do
  db <- getIndexerDb
  upsertItem BTC db knownPeersRecKey $ KnownPeersRec $ convert @_ @KnownPeerRecItem <$> peers

addKnownPeers :: (HasIndexerDB m, MonadLogger m) => [Peer] -> m ()
addKnownPeers peers = do
  db <- getIndexerDb
  let mapped = convert @_ @KnownPeerRecItem <$> peers
  stored <- getParsedExact @KnownPeersRec BTC "addKnownPeers" db knownPeersRecKey
  upsertItem BTC db knownPeersRecKey $ KnownPeersRec $ mapped ++ (unKnownPeersRec stored)

emptyKnownPeers :: (HasIndexerDB m, MonadLogger m) => m ()
emptyKnownPeers = setKnownPeersList []

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
  write db def $ putItem BTC knownPeersRecKey $ KnownPeersRec []

addBlockInfo :: (HasFiltersDB m, HasIndexerDB m, MonadLogger m)
  => BlockInfo -> BlockHeight -> m ()
addBlockInfo update to = do
  db <- getFiltersDb
  let current = blockMetaBlockHeight $ blockInfoMeta update
  let targetCurrency = blockMetaCurrency $ blockInfoMeta update
  let newBlockHash = blockMetaHeaderHashHexView $ blockInfoMeta update
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
    valueSelector info = BlockMetaRec (blockMetaHeaderHashHexView info) (blockMetaAddressFilterHexView info)

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
