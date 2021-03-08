{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Word
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.String
import Data.Maybe
import Data.Either
import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.FromRow
import Text.InterpolatedString.Perl6 (qc)
import Data.List as List

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Utxo
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types
import Ergvein.Text
import Ergvein.Types.Transaction
import qualified Data.Serialize          as S

insertBlockInfoRec :: Connection -> Currency -> BlockHeight -> BlockInfoRec -> IO ()
insertBlockInfoRec conn cur h (BlockInfoRec bhash filt) = execute conn [qc|
  insert or replace into block_info (bi_cur, bi_height, bi_hash, bi_filt) values (?,?,?,?)
  |] (fromEnum cur, h, BSS.fromShort bhash, filt)

selectBlockInfoRec :: Connection -> Currency -> BlockHeight -> IO (Maybe BlockInfoRec)
selectBlockInfoRec conn cur h = fmap listToMaybe $ query conn [qc|
  select bi_hash, bi_filt from block_info
  where bi_cur = ? and bi_height = ?
  |] (fromEnum cur, h)

selectBlockInfoRecRange :: Connection -> Currency -> BlockHeight -> BlockHeight -> IO [BlockInfoRec]
selectBlockInfoRecRange conn cur beg end = query conn [qc|
    select bi_hash, bi_filt from block_info
    where bi_cur = ?
      and bi_height >= ?
      and bi_height < ?
  |] (fromEnum cur, beg, end)

insertScannedBlockHeight :: Connection -> Currency -> BlockHeight -> IO ()
insertScannedBlockHeight conn cur h = execute conn [qc|
  insert or replace into scanned_height (sh_cur, sh_height) values (?,?)
|] (fromEnum cur, h)

selectScannedBlockHeight :: Connection -> Currency -> IO BlockHeight
selectScannedBlockHeight conn cur = fmap (fromMaybe 0 . fmap fromOnly . listToMaybe) $
  query conn [qc|
    select sh_height from scanned_height
    where sh_cur = ?
  |] (Only $ fromEnum cur)

selectTxHeight :: Connection -> TxHash -> IO (Maybe BlockHeight)
selectTxHeight conn thash = fmap (fmap fromOnly . listToMaybe) $ query conn [qc|
  select th_height from tx_height
  where th_hash = ? |] $ Only $ S.encode thash

insertTxHeights :: Connection -> BlockHeight -> [TxHash] -> IO ()
insertTxHeights conn bh hashes = do
  st <- openStatement conn [qc|
    insert or replace into tx_height
    (th_hash, th_height) values (?,?)|]
  forM_ hashes $ \txh -> do
    reset st
    bind st (S.encode txh, bh)
    _ <- nextRow st :: IO (Maybe (Only Int))
    pure ()
  closeStatement st

insertUtxoEntry :: Connection -> TxInfo -> IO ()
insertUtxoEntry conn (TxInfo thash txraw unsp) = execute conn [qc|
  insert or replace into utxo
    (utxo_txhash, utxo_txraw, utxo_txunspent) values (?,?,?);
  |] (S.encode thash, txraw, unsp)

insertUtxoBatch :: Connection -> [TxInfo] -> IO ()
insertUtxoBatch conn vals = do
  st <- openStatement conn
    [qc| insert or replace into utxo (utxo_txhash, utxo_txraw, utxo_txunspent) values (?,?,?);|]
  forM_ vals $ \(TxInfo thash txraw unsp) -> do
    reset st
    bind st (S.encode thash, txraw, unsp)
    _ <- nextRow st :: IO (Maybe (Only Int))
    pure ()
  closeStatement st

selectTxRaw :: Connection -> TxHash -> IO (Maybe ByteString)
selectTxRaw conn th = fmap (fmap fromOnly . listToMaybe) $ query conn [qc|
  select utxo_txraw from utxo
  where utxo_txhash = ? |] $ Only $ S.encode th

selectTxUnspent :: Connection -> TxHash -> IO Word32
selectTxUnspent conn th = fmap (fromMaybe 0 . fmap fromOnly . listToMaybe) $ query conn [qc|
  select utxo_txunspent from utxo
  where utxo_txhash = ? |] $ Only $ S.encode th

updateTxUnspent :: Connection -> TxHash -> Word32 -> IO ()
updateTxUnspent conn th unsp = execute conn [qc|
  update utxo
  set utxo_txunspent = ?
  where utxo_txhash = ?
|] (unsp, S.encode th)

deleteUtxoEntry :: Connection -> TxHash -> IO ()
deleteUtxoEntry conn th = execute conn [qc|
    delete from utxo where utxo_txhash = ?
  |] $ Only $ S.encode th

deleteUtxoBatch :: Connection -> [TxHash] -> IO ()
deleteUtxoBatch conn ths = execute conn [qc|
    delete from utxo where utxo_txhash in ?
  |] $ fmap S.encode ths

main :: IO ()
main = do
  conn <- open ":memory:"
  initBlockInfoRecTable conn
  initScannedHeightTable conn
  initTxHeightTable conn
  initUtxoTable conn

  execute_ conn "PRAGMA synchronous=OFF;"
  execute_ conn "pragma journal_mode = WAL;"


  let cur = BTC
      h = 123
      bh = "00000000a3bbe4fd1da16a29dbdaba01cc35d6fc74ee17f794cf3aab94f7aaa0"
      filt = "b944ef8c77f9b5f4a4276880f17256988bba4d0125abc54391548061a688ae09"
      brec = BlockInfoRec bh filt
  let th0 :: TxHash = fromRight undefined $ S.decode $ BS.pack $ [0,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
  let th1 :: TxHash = fromRight undefined $ S.decode $ BS.pack $ [0,1,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
  let th2 :: TxHash = fromRight undefined $ S.decode $ BS.pack $ [0,2,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
  let th3 :: TxHash = fromRight undefined $ S.decode $ BS.pack $ [0,3,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
  let th4 :: TxHash = fromRight undefined $ S.decode $ BS.pack $ [0,4,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
  print th0

  insertBlockInfoRec conn cur h brec
  insertBlockInfoRec conn cur (h + 1) brec
  insertBlockInfoRec conn cur (h + 1) $ BlockInfoRec bh (BS.drop 1 filt)
  insertBlockInfoRec conn cur (h + 2) brec
  mv <- selectBlockInfoRec conn cur h
  print $ (brec ==) <$> mv
  vals <- selectBlockInfoRecRange conn cur h (h+3)
  print $ (brec ==) <$> vals

  insertScannedBlockHeight conn cur h
  insertScannedBlockHeight conn cur $ h + 1
  insertScannedBlockHeight conn cur $ h + 2
  print =<< selectScannedBlockHeight conn cur


  insertTxHeights conn h [th0,th1,th2,th3]
  print =<< selectTxHeight conn th0
  print =<< selectTxHeight conn th1
  print =<< selectTxHeight conn th4


  -- insertUtxoEntry conn th0 filt 0
  -- insertUtxoEntry conn th1 filt 1
  -- insertUtxoEntry conn th2 filt 2
  -- insertUtxoEntry conn th3 filt 3

  insertUtxoBatch conn [
      TxInfo th0 filt 0
    , TxInfo th1 filt 1
    , TxInfo th2 filt 2
    , TxInfo th3 filt 3
    ]

  print =<< selectTxRaw conn th0
  print =<< selectTxUnspent conn th0

  print =<< selectTxRaw conn th2
  print =<< selectTxUnspent conn th2

  print =<< selectTxRaw conn th4
  print =<< selectTxUnspent conn th4

  putStrLn "Hello World"
