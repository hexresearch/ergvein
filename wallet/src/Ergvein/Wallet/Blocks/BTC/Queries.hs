module Ergvein.Wallet.Blocks.BTC.Queries(
    insertBtcBlock
  , insertMultipleBtcBlocks
  , getBtcBlock
  , insertBtcBlockTxHashesToBlockHash
  , insertMultipleBtcBlocksTxHashesToBlockHash
  , getBtcBlockHashByTxHash
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Maybe
import Database.LMDB.Simple
import Network.Haskoin.Block
import Network.Haskoin.Transaction

import Ergvein.Wallet.Blocks.BTC.Types
import Ergvein.Wallet.Blocks.Types
import Ergvein.Wallet.Platform

insertBtcBlock :: (MonadIO m, HasBlocksStorage m) => Block -> m ()
insertBtcBlock blk = do
  e <- getBlocksStorage
  liftIO . readWriteTransaction e $ do
    db <- getBtcBlocksDb
    dbh <- getBtcBlocksHeightDb
    put db blkhs $ Just blk
    where
      blkhs = headerHash $ blockHeader blk

insertMultipleBtcBlocks :: (MonadIO m, HasBlocksStorage m) => [Block] -> m ()
insertMultipleBtcBlocks blks = do
  e <- getBlocksStorage
  liftIO . readWriteTransaction e $ do
    db <- getBtcBlocksDb
    flip traverse_ blks $ \blk -> do
      -- liftIO $ putStrLn $ "Inserting block: " <> show (headerHash $ blockHeader blk)
      put db (headerHash $ blockHeader blk) $ Just blk

getBtcBlock :: (MonadIO m, HasBlocksStorage m) => BlockHash -> m (Maybe Block)
getBtcBlock bh = do
  e <- getBlocksStorage
  liftIO . readOnlyTransaction e $ do
    db <- getBtcBlocksDb
    get db bh

insertBtcBlockTxHashesToBlockHash :: (MonadIO m, HasBlocksStorage m) => Block -> m ()
insertBtcBlockTxHashesToBlockHash blk = do
  e <- getBlocksStorage
  liftIO . readWriteTransaction e $ do
    db <- getBtcTxsToBlocksDb
    flip traverse_ (blockTxns blk) $ \tx ->
      put db (txHash tx) $ Just (headerHash $ blockHeader blk)

insertMultipleBtcBlocksTxHashesToBlockHash :: (MonadIO m, HasBlocksStorage m) => [Block] -> m ()
insertMultipleBtcBlocksTxHashesToBlockHash blks = do
  e <- getBlocksStorage
  liftIO . readWriteTransaction e $ do
    db <- getBtcTxsToBlocksDb
    flip traverse_ blks $ \blk ->
      flip traverse_ (blockTxns blk) $ \tx -> do
        -- liftIO $ putStrLn $ "Inserting tx " <> show (txHash tx) <> " for block " <> show (headerHash $ blockHeader blk)
        put db (txHash tx) $ Just (headerHash $ blockHeader blk)

getBtcBlockHashByTxHash :: (MonadIO m, HasBlocksStorage m) => TxHash -> m (Maybe BlockHash)
getBtcBlockHashByTxHash txHash = do
  e <- getBlocksStorage
  liftIO . readOnlyTransaction e $ do
    db <- getBtcTxsToBlocksDb
    get db txHash
