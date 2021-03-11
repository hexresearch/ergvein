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
  -- *
  , selectScannedBlockHeight
  , selectTxWithUnspent
  ) where

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
import Ergvein.Types.Currency as Currency
import Ergvein.Types.Transaction

import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Short  as BSS

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

addBlockInfo :: (HasBtcRollback m, HasDbs m, MonadLogger m, MonadBaseControl IO m)
  => BlockInfo -> m ()
addBlockInfo bi@(BlockInfo meta spent txinfos) = do
  -- void $ liftIO getChar
  let (dels, upds) = HM.foldlWithKey' foo ([],[]) spent
  conMem <- getUtxoDb
  commitChan <- getCommitChannel
  insertBlockInfoRec cur height $ BlockInfoRec blkHash filt
  liftIO $ withTransaction conMem $ do
    insertUtxoBatch conMem txinfos

    executeMany conMem [qc| delete from utxo where utxo_txhash = ? |] dels

    execute conMem [qc|
      insert or replace into scan_progress values (?,?,?,?)
    |] (fromEnum cur, height, BSS.fromShort blkHash, BSS.fromShort prevHash)

    executeMany conMem [qc|
      update utxo
      set utxo_txunspent = ?
      where utxo_txhash = ?
    |] upds
    -- atomically $ writeTChan commitChan bi

  pure ()
  where
    BlockMetaInfo cur height blkHash prevHash filt = meta
    foo (del, upd) k v = if v == 0 then ((Only k):del, upd) else (del, (v, k):upd)

---

insertBlockInfoRec :: HasDbs m => Currency -> BlockHeight -> BlockInfoRec -> m ()
insertBlockInfoRec cur h (BlockInfoRec bhash filt) = do
  conn <- getFiltersDb
  liftIO $ execute conn [qc|
    insert or replace into block_info (bi_cur, bi_height, bi_hash, bi_filt) values (?,?,?,?)
    |] (fromEnum cur, h, fromShort bhash, filt)


-- selectBlockInfoRecRange :: HasDbs m => Currency -> BlockHeight -> BlockHeight -> m [BlockInfoRec]
-- selectBlockInfoRecRange cur beg end = do
--   conn <- getUtxoDb
--   liftIO $ query conn [qc|
--       select bi_hash, bi_filt from block_info
--       where bi_cur = ?
--         and bi_height >= ?
--         and bi_height < ?
--     |] (fromEnum cur, beg, end)

selectScannedBlockHeight :: HasDbs m => Currency -> m (Maybe BlockHeight)
selectScannedBlockHeight cur = do
  conn <- getUtxoDb
  liftIO $ fmap (fmap fromOnly . listToMaybe) $
    query conn [qc|
      select sp_height from scan_progress
      where sp_cur = ?
    |] (Only $ fromEnum cur)

insertUtxoBatch :: MonadIO m => Connection -> [TxInfo] -> m ()
insertUtxoBatch conn vals = liftIO $ executeMany conn [qc|
      insert or replace into utxo
      (utxo_txhash, utxo_txraw, utxo_txunspent) values (?,?,?);
    |] vals

selectTxWithUnspent :: HasDbs m => ByteString -> m (Maybe (ByteString, Word32))
selectTxWithUnspent th = do
  conn <- getUtxoDb
  liftIO $ fmap listToMaybe $ query conn [qc|
    select utxo_txraw,utxo_txunspent from utxo
    where utxo_txhash = ? |] $ Only th
