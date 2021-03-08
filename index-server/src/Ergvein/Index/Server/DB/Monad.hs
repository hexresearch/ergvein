{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Monad where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift

import Ergvein.Index.Server.DB.Schema.Indexer
import Database.SQLite.Simple

import qualified Data.Sequence as Seq

data DBTag = DBFilters | DBIndexer | DBUtxo | DBTxIndex
  deriving (Eq, Show)

class HasBtcRollback m where
  getBtcRollbackVar :: m (TVar (Seq.Seq RollbackRecItem))

class MonadIO m => HasFiltersConn m where
  getFiltersConn :: m Connection

class MonadIO m => HasUtxoConn m where
  getUtxoConn :: m Connection

class MonadIO m => HasTxIndexConn m where
  getTxIndexConn :: m Connection
