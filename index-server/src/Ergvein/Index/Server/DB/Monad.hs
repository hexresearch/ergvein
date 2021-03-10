{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Monad where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift

import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.BlockchainScanning.Types
import Database.SQLite.Simple

import qualified Data.Sequence as Seq

data DBTag = DBFilters | DBIndexer | DBUtxo | DBTxIndex
  deriving (Eq, Show)

class HasBtcRollback m where
  getBtcRollbackVar :: m (TVar (Seq.Seq RollbackRecItem))

class MonadIO m => HasDbs m where
  getFiltersDb :: m Connection
  getUtxoDb :: m Connection
  getRollDb :: m Connection
  getDbCounter :: m (TVar (Int, Int))
  getCommitChannel :: m (TChan BlockInfo)
