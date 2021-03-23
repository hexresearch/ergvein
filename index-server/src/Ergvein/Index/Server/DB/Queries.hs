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
  , deleteLastScannedBlock
  -- * Filters db queries
  , getScannedHeight
  , setScannedHeight
  , addBlockMetaInfos
  , getBlockInfoRec
  -- * Combined queries
  , addBlockInfo
  , addTxIndexInfo
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
import Data.List.Split (chunksOf)
import Data.Word
import Data.Maybe
import Data.Time.Clock
import Database.LevelDB

import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Schema.Utxo
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.DB.Wrapper
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.PeerDiscovery.Types
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
  mKnownPeers <- getParsed Currency.BTC "getKnownPeers" db knownPeersRecKey
  let knownPeers = fromMaybe (KnownPeersRec []) mKnownPeers
  currentTime <- liftIO getCurrentTime
  actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  let validDate = (-actualizationDelay) `addUTCTime` currentTime
      filteredByLastValidatedAt = filter ((validDate <=) . read . T.unpack . knownPeerRecLastValidatedAt) $ unKnownPeersRec knownPeers
  pure $ convert <$> filteredByLastValidatedAt

getPeerList :: (HasIndexerDB m, MonadLogger m) => m [Peer]
getPeerList = fmap (fmap convert . unKnownPeersRec) peerList

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
  maybeLst <- getParsed @KnownPeersRec Currency.BTC "getKnownPeersList"  idb knownPeersRecKey
  pure $ fromMaybe (KnownPeersRec mempty) maybeLst

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
  stored <- getParsed currency "BlockHeight" db $ scannedHeightKey currency
  pure $ scannedHeightRecHeight <$> stored

setScannedHeight :: (HasFiltersDB m, MonadLogger m) => Currency -> BlockHeight -> m ()
setScannedHeight currency height = do
  db <- getFiltersDb
  writeLDB db def $ putItem currency (scannedHeightKey currency) $ ScannedHeightRec height

initIndexerDb :: DB -> IO ()
initIndexerDb db = do
  write db def $ putItem Currency.BTC knownPeersRecKey $ KnownPeersRec []

addBlockInfo :: (HasBtcRollback m, HasFiltersDB m, HasIndexerDB m, HasUtxoDB m, MonadLogger m, MonadBaseControl IO m)
  => BlockInfo -> m ()
addBlockInfo (BlockInfo meta spent txinfos) = do
  dbf <- getFiltersDb
  dbu <- getUtxoDb
  writeLDB dbu def txInfosBatch
  writeLDB dbf def $ metaInfosBatch <> heightWrite
  insertRollback cur $ RollbackRecItem txHashes prevHash (height -1)
  insertSpentTxUpdates cur spent
  setLastScannedBlock cur blkHash
  where
    BlockMetaInfo cur height blkHash prevHash filt = meta
    txHashes       = txHash <$> txinfos
    txInfosBatch   = putTxInfosAsRecs cur height txinfos
    metaInfosBatch = putItem cur (blockInfoRecKey (cur, height)) $ BlockInfoRec blkHash filt
    heightWrite    = putItem cur (scannedHeightKey cur) $ ScannedHeightRec height

setLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> ShortByteString -> m ()
setLastScannedBlock currency blockHash = do
  db <- getIndexerDb
  upsertItem currency db (lastScannedBlockHeaderHashRecKey currency) $ LastScannedBlockHeaderHashRec blockHash

deleteLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> m ()
deleteLastScannedBlock currency = do
  db <- getIndexerDb
  writeLDB db def $ [LDB.Del $ lastScannedBlockHeaderHashRecKey currency]

getLastScannedBlock :: (HasIndexerDB m, MonadLogger m) => Currency -> m (Maybe ShortByteString)
getLastScannedBlock currency = do
  db <- getIndexerDb
  maybeLastScannedBlock <- getParsed currency "lastScannedBlockHeaderHashRecKey" db $ lastScannedBlockHeaderHashRecKey currency
  pure $ lastScannedBlockHeaderHashRecHash <$> maybeLastScannedBlock

-- Currency should be consistent with currency in BlockInfoMeta
addBlockMetaInfos :: (HasFiltersDB m, MonadLogger m) => Currency -> [BlockMetaInfo] -> m ()
addBlockMetaInfos currency infos = do
  db <- getFiltersDb
  writeLDB db def $ putItems currency keySelector valueSelector infos
  where
    keySelector   info = blockInfoRecKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockInfoRec (blockMetaHeaderHash info) (blockMetaAddressFilter info)

btcRollbackSize :: Int
btcRollbackSize = 64

insertRollback :: (HasBtcRollback m, MonadIO m, MonadLogger m)
  => Currency -> RollbackRecItem -> m ()
insertRollback cur = case cur of
  Currency.BTC -> insertBtcRollback
  _ -> const $ pure ()

insertBtcRollback :: (HasBtcRollback m, MonadIO m, MonadLogger m)
  => RollbackRecItem -> m ()
insertBtcRollback ritem = do
  rollVar <- getBtcRollbackVar
  liftIO $ atomically $ modifyTVar' rollVar $ \rse ->
    Seq.take btcRollbackSize $ ritem Seq.<| rse

storeRollbackSequence :: (HasIndexerDB m, MonadLogger m) => Currency -> RollbackSequence -> m ()
storeRollbackSequence cur rse = do
  idb <- getIndexerDb
  writeLDB idb def $ pure $ LDB.Put (rollbackKey cur) $ egvSerialize cur rse

loadRollbackSequence :: (HasIndexerDB m, MonadLogger m) => Currency -> m RollbackSequence
loadRollbackSequence cur = do
  idb <- getIndexerDb
  mseq <- getParsed cur "loadRollbackSequence" idb $ rollbackKey cur
  pure $ fromMaybe (RollbackSequence mempty) mseq

insertSpentTxUpdates :: (HasUtxoDB m, MonadLogger m, MonadBaseControl IO m) => Currency -> Map.Map TxHash Word32 -> m ()
insertSpentTxUpdates _ outs = do
  udb <- getUtxoDb
  let outsl = chunksOf 100 $ Map.toList outs
  upds <- fmap (mconcat . mconcat) $ mapConcurrently (traverse (mkupds udb)) outsl
  writeLDB udb def upds
  where
    maybe' mv c = maybe [] c mv
    either' ev c = either (const []) c ev
    mkupds udb (th, sp) = do
      let k = txUnspentKey th
      mraw <- getLDB udb def k
      pure $ maybe' mraw $ \bs -> either' (egvDeserialize BTC bs) $ \(TxRecUnspent unsp) ->
        if unsp <= sp
          then [LDB.Del k, LDB.Del $ txBytesKey th]
          else [LDB.Put k $ egvSerialize BTC $ TxRecUnspent (unsp - sp)]

performRollback :: (HasFiltersDB m, HasIndexerDB m, HasUtxoDB m, HasBtcRollback m, MonadLogger m) => Currency -> m Int
performRollback cur = case cur of
  Currency.BTC -> performBtcRollback
  _ -> pure 0 --TODO

performBtcRollback :: (HasFiltersDB m, HasUtxoDB m, HasIndexerDB m, HasBtcRollback m, MonadLogger m) => m Int
performBtcRollback = do
  let cur = Currency.BTC
  udb <- getUtxoDb
  idb <- getIndexerDb
  rollVar <- getBtcRollbackVar
  rse <- liftIO . readTVarIO $ rollVar
  let clearSeq = LDB.Put (rollbackKey cur) $ egvSerialize cur (RollbackSequence mempty)
  case Seq.viewr rse of
    Seq.EmptyR -> pure ()
    _ Seq.:> lst -> do
      setLastScannedBlock cur $ rollbackPrevBlockHash lst
      setScannedHeight cur $ rollbackPrevHeight lst

  let spentTxIds = fold $ rollbackItemAdded <$> rse
  let dels = mconcat $ flip fmap spentTxIds $ \th -> [LDB.Del (txBytesKey th), LDB.Del (txHeightKey th)]
  writeLDB udb def dels
  writeLDB idb def $ pure clearSeq
  liftIO $ atomically $ writeTVar rollVar mempty
  pure $ Seq.length rse

addTxIndexInfo :: (HasUtxoDB m, MonadLogger m) => Currency -> TxIndexInfo -> m ()
addTxIndexInfo cur = case cur of
  ERGO -> const $ pure ()
  BTC -> addBtcTxIndexInfo

addBtcTxIndexInfo :: (HasUtxoDB m, MonadLogger m) => TxIndexInfo -> m ()
addBtcTxIndexInfo tinfo@TxIndexInfo{..} = do
  dbu <- getUtxoDb
  writeLDB dbu def $ putTxIndexInfoAsRec BTC tinfo

getBlockInfoRec :: (HasFiltersDB m, MonadLogger m) => Currency -> BlockHeight -> m (Maybe BlockInfoRec)
getBlockInfoRec c h = do
  fdb <- getFiltersDb
  getParsed c "getBlockInfoRec" fdb $ blockInfoRecKey (c,h)
