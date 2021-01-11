{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Monad where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.IO.Unlift
import Control.Monad.Reader

import Ergvein.Index.Server.DB.Schema.Indexer

import qualified Data.Sequence as Seq
import qualified Database.LevelDB as LDB

class  MonadIO m => HasFiltersDB m where
  getFiltersDbVar :: m (MVar LDB.DB)

class MonadIO m => HasIndexerDB m where
  getIndexerDbVar :: m (MVar LDB.DB)

class MonadIO m => HasLMDBs m where
  getDb :: DBTag -> m LDB.DB

data DBTag = DBFilters | DBIndexer
  deriving (Eq, Show)

class HasBtcRollback m where
  getBtcRollbackVar :: m (TVar (Seq.Seq RollbackRecItem))

instance MonadIO m => HasIndexerDB (ReaderT (MVar LDB.DB) m) where
  getIndexerDbVar = ask

instance MonadIO m => HasFiltersDB (ReaderT (MVar LDB.DB) m) where
  getFiltersDbVar  = ask

readFiltersDb :: HasFiltersDB m => m LDB.DB
readFiltersDb = liftIO . readMVar =<< getFiltersDbVar

readIndexerDb :: HasIndexerDB m => m LDB.DB
readIndexerDb = liftIO . readMVar =<< getIndexerDbVar
