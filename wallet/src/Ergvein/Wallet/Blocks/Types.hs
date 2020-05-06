module Ergvein.Wallet.Blocks.Types(
    BlocksStorage
  , HasBlocksStorage(..)
  ) where

import Control.Monad.Reader
import Database.LMDB.Simple

type BlocksStorage = Environment ReadWrite

class Monad m => HasBlocksStorage m where
  getBlocksStorage :: m BlocksStorage

instance Monad m => HasBlocksStorage (ReaderT BlocksStorage m) where
  getBlocksStorage = ask
  {-# INLINE getBlocksStorage #-}
