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
  -- * Filters db queries
  , getScannedHeight
  , setScannedHeight
  , addBlockMetaInfos
  -- * Combined queries
  , addBlockInfo
  -- * Rollback related
  , storeRollbackSequence
  , loadRollbackSequence
  , performRollback
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Conversion
import Data.ByteString.Short (ShortByteString)
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Time.Clock
import Database.LevelDB

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.Utils
import Ergvein.Types.Currency as Currency
import Ergvein.Types.Transaction

import qualified Data.Map.Strict  as Map
import qualified Data.Sequence    as Seq
import qualified Data.Text        as T
import qualified Database.LevelDB as LDB

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

getPeerList :: (HasIndexerDB m, MonadLogger m) => m [Peer]
getPeerList = do
  idb <- getIndexerDb
  maybePeers <- getParsed  @KnownPeersRec Currency.BTC "getKnownPeersList"  idb knownPeersRecKey
  pure $ convert <$> maybe mempty unKnownPeersRec maybePeers

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

addBlockInfo :: (HasBtcRollback m, HasFiltersDB m, HasIndexerDB m, MonadLogger m, MonadBaseControl IO m) => BlockInfo -> m ()
addBlockInfo update = do
  db <- getFiltersDb
  let targetCurrency = blockMetaCurrency $ blockInfoMeta update
  let newBlockHash = blockMetaHeaderHash $ blockInfoMeta update
  write db def $ putTxInfosAsRecs targetCurrency (blockContentTxInfos update)
  insertRollback targetCurrency $ RollbackRecItem
    (txHash <$> blockContentTxInfos update)
    (spentTxOutputs update)
    (blockMetaPreviousHeaderBlockHash $ blockInfoMeta update)
    (blockMetaBlockHeight (blockInfoMeta update) - 1)
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

btcRollbackSize :: Int
btcRollbackSize = 64

insertRollback :: (HasBtcRollback m, HasFiltersDB m, MonadLogger m, MonadBaseControl IO m)
  => Currency -> RollbackRecItem -> m ()
insertRollback cur = case cur of
  Currency.BTC -> insertBtcRollback
  _ -> const $ pure ()

insertBtcRollback :: (HasBtcRollback m, HasFiltersDB m, MonadLogger m, MonadBaseControl IO m)
  => RollbackRecItem -> m ()
insertBtcRollback ritem = do
  rollVar <- getBtcRollbackVar
  rse <- liftIO $ readTVarIO rollVar
  let rse' = ritem Seq.<| rse
  if Seq.length rse' <= btcRollbackSize
    then liftIO $ atomically $ writeTVar rollVar rse'
    else do
      let rest Seq.:> lst = Seq.viewr rse'
      finalizeRollbackItem Currency.BTC lst
      liftIO $ atomically $ writeTVar rollVar rest

storeRollbackSequence :: (HasIndexerDB m, MonadLogger m) => Currency -> RollbackSequence -> m ()
storeRollbackSequence cur rse = do
  idb <- getIndexerDb
  write idb def $ pure $ LDB.Put (rollbackKey cur) $ egvSerialize cur rse

loadRollbackSequence :: (HasIndexerDB m, MonadLogger m) => Currency -> m RollbackSequence
loadRollbackSequence cur = do
  idb <- getIndexerDb
  mseq <- getParsed cur "loadRollbackSequence" idb $ rollbackKey cur
  pure $ fromMaybe (RollbackSequence mempty) mseq

finalizeRollbackItem :: (HasFiltersDB m, MonadLogger m, MonadBaseControl IO m) => Currency -> RollbackRecItem -> m ()
finalizeRollbackItem _cur (RollbackRecItem _ outs _ _) = do
  fdb <- getFiltersDb
  let outsl = mkChunks 100 $ Map.toList outs
  upds <- fmap (mconcat . mconcat) $ mapConcurrently (traverse (mkupds fdb)) outsl
  write fdb def upds
  where
    maybe' mv c = maybe [] c mv
    either' ev c = either (const []) c ev
    mkupds fdb (th, sp) = do
      let k = txMetaKey th
      mraw <- get fdb def k
      pure $ maybe' mraw $ \bs -> either' (deserializeWord32 bs) $ \unsp -> let
        o = unsp - sp
        in if o <= 0 then [LDB.Del k, LDB.Del $ txRawKey th] else [LDB.Put k (serializeWord32 o)]

performRollback :: (HasFiltersDB m, HasIndexerDB m, HasBtcRollback m, MonadLogger m) => Currency -> m Int
performRollback cur = case cur of
  Currency.BTC -> performBtcRollback
  _ -> pure 0 --TODO

performBtcRollback :: (HasFiltersDB m, HasIndexerDB m, HasBtcRollback m, MonadLogger m) => m Int
performBtcRollback = do
  let cur = Currency.BTC
  fdb <- getIndexerDb
  idb <- getFiltersDb
  rollVar <- getBtcRollbackVar
  rse <- liftIO . readTVarIO $ rollVar
  let clearSeq = LDB.Put (rollbackKey cur) $ egvSerialize cur (RollbackSequence mempty)
  case Seq.viewr rse of
    Seq.EmptyR -> pure ()
    _ Seq.:> lst -> do
      setLastScannedBlock cur $ rollbackPrevBlockHash lst
      setScannedHeight cur $ rollbackPrevHeight lst

  let spentTxIds = fold $ rollbackItemAdded <$> rse
  let dels = mconcat $ flip fmap spentTxIds $ \th -> [LDB.Del (txRawKey th), LDB.Del (txMetaKey th)]
  write fdb def dels
  write idb def $ pure clearSeq
  liftIO $ atomically $ writeTVar rollVar mempty
  pure $ Seq.length rse
