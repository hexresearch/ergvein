module Ergvein.Index.Server.DB.Queries
  (
    -- * Indexer db queries
    getActualPeers
  , getPeerList
  , setPeerList
  , upsertPeer
  , emptyKnownPeers
  , deletePeerBySockAddr
  -- -- * Combined queries
  , addBlockInfo
  , addTxIndexInfo
  -- * Rollback related
  , storeRollbackSequence
  , loadRollbackSequence
  , performRollback
  -- *
  , insertBlockInfoRec
  , selectBlockInfoRec
  , selectBlockInfoRecRange
  , insertScannedBlockHeight
  , selectScannedBlockHeight
  , selectTxHeight
  , insertTxHeights
  , selectLastTxHeight
  , insertLastTxHeight
  , insertUtxoEntry
  , insertUtxoBatch
  , selectTxRaw
  , selectTxUnspent
  , updateTxUnspent
  , deleteUtxoEntry
  , deleteUtxoBatch
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.ByteString.Short (fromShort)
import Data.ByteString (ByteString)
import Data.Word
import Data.Maybe

import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.Utils
import Ergvein.Types.Currency as Currency
import Ergvein.Types.Transaction

import qualified Data.Map.Strict  as Map
import qualified Data.Sequence    as Seq

import qualified Data.Serialize   as S
import Database.SQLite.Simple hiding (fold)
import Text.InterpolatedString.Perl6 (qc)

getActualPeers :: (MonadLogger m, HasDiscoveryRequisites m) =>  m [Address]
getActualPeers = pure []
  -- db <- getIndexerDb
  -- -- I put BTC here and downstream, because it doesnt actually matter but we still need a value
  -- mKnownPeers <- getParsed Currency.BTC "getKnownPeers" db knownPeersRecKey
  -- let knownPeers = fromMaybe (KnownPeersRec []) mKnownPeers
  -- currentTime <- liftIO getCurrentTime
  -- actualizationDelay <- (/1000000) . fromIntegral . descReqActualizationDelay <$> getDiscoveryRequisites
  -- let validDate = (-actualizationDelay) `addUTCTime` currentTime
  --     filteredByLastValidatedAt = filter ((validDate <=) . read . T.unpack . knownPeerRecLastValidatedAt) $ unKnownPeersRec knownPeers
  -- pure $ convert <$> filteredByLastValidatedAt

getPeerList :: (MonadLogger m) => m [Peer]
getPeerList = pure [] --fmap (fmap convert . unKnownPeersRec) peerList

setPeerList :: (MonadLogger m) => [Peer] -> m ()
setPeerList _ = pure ()
  -- idb <- getIndexerDb
  -- upsertItem Currency.BTC  idb knownPeersRecKey $ KnownPeersRec $ convert @_ @KnownPeerRecItem <$> peers

upsertPeer :: (MonadLogger m) => Peer -> m ()
upsertPeer _ = pure ()
  -- currentList <- peerList
  -- let peerRec = convert peer
  -- setPeerRecList $ KnownPeersRec $ peerRec : excludePeerByAddr (knownPeerRecAddr peerRec) (unKnownPeersRec currentList)

peerList :: (MonadLogger m) => m KnownPeersRec
peerList = pure $ KnownPeersRec mempty
  -- idb <- getIndexerDb
  -- maybeLst <- getParsed @KnownPeersRec Currency.BTC "getKnownPeersList"  idb knownPeersRecKey
  -- pure $ fromMaybe (KnownPeersRec mempty) maybeLst

setPeerRecList :: (MonadLogger m) => KnownPeersRec -> m ()
setPeerRecList _ = pure ()
  -- idb <- getIndexerDb
  -- upsertItem Currency.BTC idb knownPeersRecKey peers

deletePeerBySockAddr :: (MonadLogger m) => PeerAddr -> m ()
deletePeerBySockAddr addr = do
  currentList <- peerList
  setPeerRecList $ KnownPeersRec $ excludePeerByAddr addr  $ unKnownPeersRec currentList

excludePeerByAddr :: PeerAddr ->  [KnownPeerRecItem] -> [KnownPeerRecItem]
excludePeerByAddr addr = filter ((== addr) . knownPeerRecAddr)

emptyKnownPeers :: MonadLogger m => m ()
emptyKnownPeers = setPeerRecList $ KnownPeersRec []

addBlockInfo :: (HasBtcRollback m, HasFiltersConn m, HasUtxoConn m, HasTxIndexConn m, MonadLogger m, MonadBaseControl IO m)
  => BlockInfo -> m ()
addBlockInfo (BlockInfo meta spent txinfos) = do
  utxoConn <- getUtxoConn
  txIndexConn <- getTxIndexConn
  let boo = liftIO $ withTransaction txIndexConn $ insertTxHeights txIndexConn height txHashes
  let foo = void $ liftIO $ withTransaction utxoConn $ runConcurrently $ sequenceA [
                Concurrently $ insertUtxoBatch utxoConn txinfos
              , Concurrently $ insertSpentTxUpdates utxoConn cur spent
              ]
  void $ runConcurrently $ sequenceA [
      Concurrently $ foo
    , Concurrently boo
    , Concurrently $ insertBlockInfoRec cur height $ BlockInfoRec blkHash filt
    , Concurrently $ insertScannedBlockHeight cur height
    , Concurrently $ insertRollback cur $ RollbackRecItem txHashes prevHash (height -1)
    ]
  where
    BlockMetaInfo cur height blkHash prevHash filt = meta
    txHashes       = txHash <$> txinfos

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

storeRollbackSequence :: (MonadLogger m) => Currency -> RollbackSequence -> m ()
storeRollbackSequence _ _ = pure ()
  -- idb <- getIndexerDb
  -- writeLDB idb def $ pure $ LDB.Put (rollbackKey cur) $ egvSerialize cur rse

loadRollbackSequence :: (MonadLogger m) => Currency -> m RollbackSequence
loadRollbackSequence _ = pure $ RollbackSequence mempty
  -- idb <- getIndexerDb
  -- mseq <- getParsed cur "loadRollbackSequence" idb $ rollbackKey cur
  -- pure $ fromMaybe (RollbackSequence mempty) mseq

performRollback :: (HasBtcRollback m, MonadLogger m) => Currency -> m Int
performRollback cur = case cur of
  Currency.BTC -> performBtcRollback
  _ -> pure 0 --TODO

performBtcRollback :: (HasBtcRollback m, MonadLogger m) => m Int
performBtcRollback = pure 0
  -- let cur = Currency.BTC
  -- udb <- getUtxoDb
  -- idb <- getIndexerDb
  -- rollVar <- getBtcRollbackVar
  -- rse <- liftIO . readTVarIO $ rollVar
  -- let clearSeq = LDB.Put (rollbackKey cur) $ egvSerialize cur (RollbackSequence mempty)
  -- case Seq.viewr rse of
  --   Seq.EmptyR -> pure ()
  --   _ Seq.:> lst -> do
  --     setLastScannedBlock cur $ rollbackPrevBlockHash lst
  --     setScannedHeight cur $ rollbackPrevHeight lst
  --
  -- let spentTxIds = fold $ rollbackItemAdded <$> rse
  -- let dels = mconcat $ flip fmap spentTxIds $ \th -> [LDB.Del (txBytesKey th), LDB.Del (txHeightKey th)]
  -- writeLDB udb def dels
  -- writeLDB idb def $ pure clearSeq
  -- liftIO $ atomically $ writeTVar rollVar mempty
  -- pure $ Seq.length rse

addTxIndexInfo :: (HasTxIndexConn m, MonadLogger m) => Currency -> TxIndexInfo -> m ()
addTxIndexInfo cur = case cur of
  ERGO -> const $ pure ()
  BTC -> addBtcTxIndexInfo

addBtcTxIndexInfo :: (HasTxIndexConn m, MonadLogger m) => TxIndexInfo -> m ()
addBtcTxIndexInfo TxIndexInfo{..} = do
  conn <- getTxIndexConn
  insertTxHeights conn txIndexInfoHeight txIndexInfoIds

---

insertBlockInfoRec :: HasFiltersConn m => Currency -> BlockHeight -> BlockInfoRec -> m ()
insertBlockInfoRec cur h (BlockInfoRec bhash filt) = do
  conn <- getFiltersConn
  liftIO $ execute conn [qc|
    insert or replace into block_info (bi_cur, bi_height, bi_hash, bi_filt) values (?,?,?,?)
    |] (fromEnum cur, h, fromShort bhash, filt)

selectBlockInfoRec :: HasFiltersConn m => Currency -> BlockHeight -> m (Maybe BlockInfoRec)
selectBlockInfoRec cur h = do
  conn <- getFiltersConn
  liftIO $ fmap listToMaybe $ query conn [qc|
    select bi_hash, bi_filt from block_info
    where bi_cur = ? and bi_height = ?
    |] (fromEnum cur, h)

selectBlockInfoRecRange :: HasFiltersConn m => Currency -> BlockHeight -> BlockHeight -> m [BlockInfoRec]
selectBlockInfoRecRange cur beg end = do
  conn <- getFiltersConn
  liftIO $ query conn [qc|
      select bi_hash, bi_filt from block_info
      where bi_cur = ?
        and bi_height >= ?
        and bi_height < ?
    |] (fromEnum cur, beg, end)

insertScannedBlockHeight :: HasFiltersConn m => Currency -> BlockHeight -> m ()
insertScannedBlockHeight cur h = do
  conn <- getFiltersConn
  liftIO $ execute conn [qc|
    insert or replace into scanned_height (sh_cur, sh_height) values (?,?)
    |] (fromEnum cur, h)

selectScannedBlockHeight :: HasFiltersConn m => Currency -> m (Maybe BlockHeight)
selectScannedBlockHeight cur = do
  conn <- getFiltersConn
  liftIO $ fmap (fmap fromOnly . listToMaybe) $
    query conn [qc|
      select sh_height from scanned_height
      where sh_cur = ?
    |] (Only $ fromEnum cur)

selectTxHeight :: HasTxIndexConn m => TxHash -> m (Maybe BlockHeight)
selectTxHeight thash = do
  conn <- getTxIndexConn
  liftIO $ fmap (fmap fromOnly . listToMaybe) $ query conn [qc|
    select th_height from tx_height
    where th_hash = ? |] $ Only $ S.encode thash

insertTxHeights :: MonadIO m => Connection -> BlockHeight -> [TxHash] -> m ()
insertTxHeights conn bh hashes = liftIO $ do
  forM_ hashes $ \txh -> execute conn [qc|
    insert or replace into tx_height
      (th_hash, th_height) values (?,?)
    |] (S.encode txh, bh)
  case hashes of
    [] -> pure ()
    t:_ -> insertLastTxHeight conn (getCur t) bh
  where
    getCur t = case t of
      BtcTxHash{} -> BTC
      ErgTxHash{} -> ERGO

insertUtxoEntry :: HasUtxoConn m => TxInfo -> m ()
insertUtxoEntry (TxInfo thash txraw unsp) = do
  conn <- getUtxoConn
  liftIO $ execute conn [qc|
      insert or replace into utxo
      (utxo_txhash, utxo_txraw, utxo_txunspent) values (?,?,?);
    |] (S.encode thash, txraw, unsp)

insertUtxoBatch :: MonadIO m => Connection -> [TxInfo] -> m ()
insertUtxoBatch conn vals = liftIO $ forM_ vals $ \(TxInfo thash txraw unsp) ->
  execute conn [qc|
      insert or replace into utxo
      (utxo_txhash, utxo_txraw, utxo_txunspent) values (?,?,?);
    |] (S.encode thash, txraw, unsp)

selectTxRaw :: HasUtxoConn m => TxHash -> m (Maybe ByteString)
selectTxRaw th = do
  conn <- getUtxoConn
  liftIO $ fmap (fmap fromOnly . listToMaybe) $ query conn [qc|
    select utxo_txraw from utxo
    where utxo_txhash = ? |] $ Only $ S.encode th

selectTxUnspent :: HasUtxoConn m => TxHash -> m Word32
selectTxUnspent th = do
  conn <- getUtxoConn
  liftIO $ fmap (fromMaybe 0 . fmap fromOnly . listToMaybe) $ query conn [qc|
    select utxo_txunspent from utxo
    where utxo_txhash = ? |] $ Only $ S.encode th

updateTxUnspent :: HasUtxoConn m => TxHash -> Word32 -> m ()
updateTxUnspent th unsp = do
  conn <- getUtxoConn
  liftIO $ execute conn [qc|
      update utxo
      set utxo_txunspent = ?
      where utxo_txhash = ?
    |] (unsp, S.encode th)

deleteUtxoEntry :: HasUtxoConn m => TxHash -> m ()
deleteUtxoEntry th = do
  conn <- getUtxoConn
  liftIO $ execute conn [qc|
      delete from utxo where utxo_txhash = ?
    |] $ Only $ S.encode th

deleteUtxoBatch :: HasUtxoConn m => [TxHash] -> m ()
deleteUtxoBatch ths = do
  conn <- getUtxoConn
  liftIO $ execute conn [qc|
      delete from utxo where utxo_txhash in ?
    |] $ fmap S.encode ths

insertSpentTxUpdates :: MonadIO m
  => Connection -> Currency -> Map.Map TxHash Word32 -> m ()
insertSpentTxUpdates conn _ outs = liftIO $ do
  let outsl = mkChunks 100 $ Map.toList outs
  upds <- fmap mconcat $ mapConcurrently (traverse mkupds) outsl
  forM_ upds $ \(th, val) -> if val <= 0
    then execute conn
      [qc| delete from utxo where utxo_txhash = ? |] $ Only th
    else execute conn
      [qc| update utxo set utxo_txunspent = ? where utxo_txhash = ? |] (val, th)

  where
    mkupds (th, sp) = do
      let th' = S.encode th
      v <- query conn
        [qc| select utxo_txunspent from utxo where utxo_txhash = ? |] $ Only th'
      let unsp = fromMaybe 0 . fmap fromOnly . listToMaybe $ v
      pure $ (th', unsp - sp)

selectLastTxHeight :: HasTxIndexConn m => Currency -> m (Maybe BlockHeight)
selectLastTxHeight cur = do
  conn <- getTxIndexConn
  liftIO $ fmap (fmap fromOnly . listToMaybe) $ query conn [qc|
    select tlh_height from tx_last_height
    where tlh_cur = ? |] $ Only $ fromEnum cur

insertLastTxHeight :: MonadIO m => Connection -> Currency -> BlockHeight -> m ()
insertLastTxHeight conn cur bh = liftIO $ execute conn [qc|
  insert or replace into tx_last_height (tlh_cur, tlh_height) values (?,?);
  |] $ (fromEnum cur, bh)
