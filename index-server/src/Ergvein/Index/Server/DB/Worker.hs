{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Worker
  (
    dbWorker
  , initDbs
  , dumpRollbackDb
  , loadRollbackDb
  ) where

import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Trans.Control
import Data.Maybe
import Data.ByteString(ByteString)
import Data.List(sortOn)
import Database.SQLite.Simple
import Text.InterpolatedString.Perl6 (qc)

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Schema.Utxo
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.Dependencies
import Ergvein.Types.Transaction

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap as IM
import qualified Data.ByteString.Short as BSS

initDbs :: MonadIO m => FilePath -> FilePath -> FilePath -> m (Connection,Connection,Connection, TVar (Int,Int))
initDbs filtersFile utxoFile rollFile = liftIO $ do
  -- Open and initialize file dbs
  filtsConn <- open filtersFile
  initBlockInfoRecTable filtsConn
  execute_ filtsConn "PRAGMA synchronous=NORMAL;"
  execute_ filtsConn "pragma journal_mode = WAL;"

  execute_ filtsConn [qc|
      CREATE TABLE IF NOT EXISTS block_info (
        bi_cur INTEGER NOT NULL,
        bi_height INTEGER NOT NULL,
        bi_hash BLOB NOT NULL,
        bi_filt BLOB NOT NULL,
        PRIMARY KEY (bi_cur, bi_height));
    |]

  -- utxoFileConn <- open utxoFile
  -- initScanProgresTable utxoFileConn
  -- initUtxoTable utxoFileConn
  -- close utxoFileConn

  rollFileConn <- open rollFile
  initRollbackDb rollFileConn
  close rollFileConn


  -- Open and initialize utxo in-memory db
  -- utxoConn <- open ":memory:"
  utxoConn <- open utxoFile
  initScanProgresTable utxoConn
  initUtxoTable utxoConn
  execute_ utxoConn "PRAGMA synchronous=NORMAL;"
  -- execute_ utxoConn "pragma journal_mode = WAL;"

  -- execute_ utxoConn [qc|attach '{utxoFile}' as udisk|]
  -- execute_ utxoConn [qc|attach '{rollFile}' as rdisk|]

  -- loadUtxoDb utxoConn

  -- execute_ utxoConn [qc|detach udisk|]
  -- execute_ utxoConn [qc|detach rdisk|]

  rollConn <- open ":memory:"
  -- initRollbackDb rollConn
  execute_ rollConn "PRAGMA synchronous=NORMAL;"
  execute_ rollConn "pragma journal_mode = WAL;"
  execute_ rollConn [qc| attach '{rollFile}' as rdisk|]
  execute_ rollConn [qc| attach '{utxoFile}' as udisk|]

  -- cntVar <- loadRollbackDb rollConn
  cntVar <- newTVarIO (0,0)

  pure (filtsConn, utxoConn, rollConn, cntVar)

initRollbackDb :: MonadIO m => Connection -> m ()
initRollbackDb conn = liftIO $ do
  execute_ conn [qc|
    create table if not exists roll_1(
      txhash BLOB PRIMARY KEY,
      txraw BLOB NOT NULL,
      txunspent INT NOT NULL );
  |]

  execute_ conn [qc|
    create table if not exists roll_2(
      txhash BLOB PRIMARY KEY,
      txraw BLOB NOT NULL,
      txunspent INT NOT NULL );
  |]

  execute_ conn [qc|
    CREATE TABLE IF NOT EXISTS scan_progress (
      sh_which INTEGER NOT NULL,
      sh_cur INTEGER NOT NULL,
      sh_height INTEGER NOT NULL,
      sh_blk_hash BLOB NOT NULL,
      sh_last_hash BLOB NOT NULL,
      sh_count INTEGER NOT NULL,
      PRIMARY KEY (sh_which, sh_cur)
      );
  |]

commitRollTableToDisk :: MonadIO m => Connection -> Int -> m ()
commitRollTableToDisk conn tabNum = liftIO $ withTransaction conn $ do
  execute_ conn [qc|
    insert or replace into udisk.utxo (utxo_txhash, utxo_txraw, utxo_txunspent)
    select txhash,txraw,txunspent from {t} where {t}.txunspent != 0
  |]

  execute conn [qc|
    insert or replace into udisk.scan_progress (sp_cur, sp_height, sp_blk_hash, sp_last_hash)
    select sh_cur,sh_height,sh_blk_hash,sh_last_hash
    from scan_progress where sh_which = ?
  |] $ Only tabNum

  execute_ conn [qc|
    delete from udisk.utxo
    where udisk.utxo.utxo_txhash in (
      select {t}.txhash from {t}
      where {t}.txunspent = 0)
  |]

  execute_ conn [qc|delete from {t}|]
  execute conn [qc|delete from scan_progress where sh_which = ?|] $ Only tabNum

  where
    t :: String = if tabNum == 1 then "roll_1" else "roll_2"

commitBlockToRollTable :: MonadIO m => Connection -> Int -> Int -> BlockInfo -> m ()
commitBlockToRollTable conn tabNum cnt (BlockInfo meta spent txinfos) = liftIO $ withTransaction conn $ do
  -- v <- query conn [qc| select sh_height from scan_progress where sh_which = ? |] $ Only tabNum
  -- let scanHeight :: BlockHeight = fromMaybe 0 . fmap fromOnly . listToMaybe $ v
  -- when (height > scanHeight) $ do
  executeMany conn [qc|
    insert or replace into {t} (txhash, txraw, txunspent) values (?,?,?)
  |] txinfos
  executeMany conn [qc|
    update {t}
    set txunspent = ?
    where txhash = ?
  |] $ fmap (\(h,v) -> (v,h)) $ HM.toList spent
  execute conn [qc|
    insert or replace into scan_progress
      (sh_which, sh_cur, sh_height, sh_blk_hash, sh_last_hash,sh_count)
    values (?,?,?,?,?,?)
  |] (tabNum, fromEnum cur, height, BSS.fromShort blkHash, BSS.fromShort prevHash, cnt + 1)
  where
    BlockMetaInfo cur height blkHash prevHash _ = meta
    t :: String = if tabNum == 1 then "roll_1" else "roll_2"

dbWorker :: (
    HasDbs m
  , HasShutdownSignal m
  , MonadUnliftIO m
  , MonadLogger m
  , MonadCatch m
  , MonadBaseControl IO m) => m Thread
dbWorker = create $ logOnException "ratesScanner" . \thread -> do
  void $ fork $ interruptThreadOnShutdown thread
  commitChan <- getCommitChannel
  conn <- getRollDb
  cntVar <- getDbCounter
  forever $ liftIO $ do
    info <- atomically $ readTChan commitChan
    (currentTab, _) <- readTVarIO $ cntVar
    v <- query conn [qc|select sh_count from scan_progress where sh_which = ?|] $ Only currentTab
    let cnt = fromMaybe 0 . fmap fromOnly . listToMaybe $ v
    let otherTab = if currentTab == 1 then 2 else 1
    if cnt >= 64
      then do
        commitRollTableToDisk conn otherTab
        commitBlockToRollTable conn otherTab 0 info
        atomically $ writeTVar cntVar (otherTab, 1)
      else do
        commitBlockToRollTable conn currentTab cnt info
        atomically $ writeTVar cntVar (currentTab, cnt + 1)

dumpRollbackDb :: MonadIO m => Connection -> m ()
dumpRollbackDb conn = liftIO $ withTransaction conn $ do
  execute_ conn [qc|
    insert or replace into rdisk.roll_1 (txhash, txraw, txunspent)
    select txhash,txraw,txunspent from roll_1;
  |]

  execute_ conn [qc|
    insert or replace into rdisk.roll_2 (txhash, txraw, txunspent)
    select txhash,txraw,txunspent from roll_2;
  |]

  execute_ conn [qc|
    insert or replace into rdisk.scan_progress select * from scan_progress;
  |]

loadRollbackDb :: MonadIO m => Connection -> m (TVar (Int,Int))
loadRollbackDb conn = liftIO $ do
  execute_ conn [qc|
    insert or replace into roll_1 (txhash, txraw, txunspent)
    select txhash,txraw,txunspent from rdisk.roll_1;
  |]
  execute_ conn [qc|
    insert or replace into roll_2 (txhash, txraw, txunspent)
    select txhash,txraw,txunspent from rdisk.roll_2;
  |]

  execute_ conn [qc|
    insert or replace into scan_progress (sh_which, sh_cur, sh_height, sh_blk_hash, sh_last_hash, sh_count)
    select sh_which,sh_cur,sh_height,sh_blk_hash,sh_last_hash,sh_count
    from rdisk.scan_progress;
  |]

  vals :: [(Int,Int)] <- query_ conn [qc| select sh_which,sh_count from scan_progress; |]

  let initVal = fromMaybe (1,0) $ listToMaybe $ sortOn snd vals

  newTVarIO initVal

loadUtxoDb :: MonadIO m => Connection -> m ()
loadUtxoDb conn = liftIO $ do
  execute_ conn [qc|
    insert or replace into utxo select * from udisk.utxo;
  |]

  execute_ conn [qc|
    insert or replace into scan_progress select * from udisk.scan_progress
  |]

  -- execute_ conn [qc|
  --   insert or replace into utxo select * from rdisk.roll_1;
  -- |]
  --
  -- execute_ conn [qc|
  --   insert or replace into utxo select * from rdisk.roll_2;
  -- |]

  -- vals :: [(Int,BlockHeight,ByteString,ByteString)] <- query_ conn [qc|
  --   select sh_cur,sh_height,sh_blk_hash,sh_last_hash from rdisk.scan_progress;
  -- |]
  --
  -- -- group by currency and pick the row with the highest height
  -- let vals' = fmap conv2 $ IM.toList $ IM.fromListWith pick $ conv1 <$> vals
  --
  -- executeMany conn [qc|
  --   insert or replace into scan_progress (sp_cur, sp_height, sp_blk_hash, sp_last_hash) values (?,?,?,?)
  -- |] vals'
  pure ()

  where
    conv1 (a,b,c,d) = (a,(b,c,d))
    conv2 (a,(b,c,d)) = (a,b,c,d)
    pick :: (BlockHeight, ByteString, ByteString) -> (BlockHeight, ByteString, ByteString) -> (BlockHeight, ByteString, ByteString)
    pick (h1,x1,y1) (h2,x2,y2) = if h1 >= h2 then (h1,x1,y1) else (h2,x2,y2)
