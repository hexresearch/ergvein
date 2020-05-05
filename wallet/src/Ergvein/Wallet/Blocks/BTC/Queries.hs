module Ergvein.Wallet.Blocks.BTC.Queries(
    insertBTCBlock
  , getBTCBlock
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Maybe
import Database.LMDB.Simple
import Network.Haskoin.Block

import Ergvein.Wallet.Blocks.BTC.Types
import Ergvein.Wallet.Blocks.Types
import Ergvein.Wallet.Platform

insertBTCBlock :: (MonadIO m, HasBlocksStorage m) => Block -> m ()
insertBTCBlock blk = do
  e <- getBlocksStorage
  liftIO . readWriteTransaction e $ do
    db <- getBTCBlocksDb
    put db (headerHash $ blockHeader blk) $ Just blk

insertMultipleBTCBlocks :: (MonadIO m, HasBlocksStorage m) => [Block] -> m ()
insertMultipleBTCBlocks blks = do
  e <- getBlocksStorage
  liftIO . readWriteTransaction e $ do
    db <- getBTCBlocksDb
    flip traverse_ blks $ \blk ->
      put db (headerHash $ blockHeader blk) $ Just blk

getBTCBlock :: (MonadIO m, HasBlocksStorage m) => BlockHash -> m (Maybe Block)
getBTCBlock bh = do
  e <- getBlocksStorage
  liftIO . readOnlyTransaction e $ do
    db <- getBTCBlocksDb
    get db bh
