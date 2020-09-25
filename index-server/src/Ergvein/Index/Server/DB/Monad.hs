{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Monad where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Reader

import Ergvein.Index.Server.DB.Schema.Indexer

import qualified Data.Sequence as Seq
import qualified Database.LevelDB as LDB

class  MonadIO m => HasFiltersDB m where
  getFiltersDb :: m LDB.DB

class MonadIO m => HasIndexerDB m where
  getIndexerDb :: m LDB.DB

class MonadIO m => HasLMDBs m where
  getDb :: DBTag -> m LDB.DB

data DBTag = DBFilters | DBIndexer
  deriving (Eq, Show)

class HasBtcRollback m where
  getBtcRollbackVar :: m (TVar (Seq.Seq RollbackRecItem))

instance MonadIO m => HasIndexerDB (ReaderT LDB.DB m) where
  getIndexerDb = ask

instance MonadIO m => HasFiltersDB (ReaderT LDB.DB m) where
  getFiltersDb = ask
