{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Monad where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift
import Control.Monad.Reader

import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Wrapper

import qualified Data.Sequence as Seq
import qualified Database.LevelDB as LDB

class  MonadIO m => HasFiltersDB m where
  getFiltersDb :: m LevelDB

class MonadIO m => HasIndexerDB m where
  getIndexerDb :: m LevelDB

class MonadIO m => HasUtxoDB m where
  getUtxoDbVar :: m (MVar LDB.DB)

class MonadIO m => HasLMDBs m where
  getDb :: DBTag -> m LDB.DB

data DBTag = DBFilters | DBIndexer | DBUtxo
  deriving (Eq, Show)

class HasBtcRollback m where
  getBtcRollbackVar :: m (TVar (Seq.Seq RollbackRecItem))

instance MonadIO m => HasIndexerDB (ReaderT LevelDB m) where
  getIndexerDb = ask

instance MonadIO m => HasFiltersDB (ReaderT LevelDB m) where
  getFiltersDb  = ask

instance MonadIO m => HasUtxoDB (ReaderT (MVar LDB.DB) m) where
  getUtxoDbVar  = ask

data AllDbVars = AllDbVars {
  allDBFilters :: MVar LDB.DB
, allDBIndexer :: MVar LDB.DB
, allDBUtxo    :: MVar LDB.DB
}

instance MonadIO m => HasIndexerDB (ReaderT AllDbVars m) where
  getIndexerDbVar = asks allDBFilters

instance MonadIO m => HasFiltersDB (ReaderT AllDbVars m) where
  getFiltersDbVar  = asks allDBIndexer

instance MonadIO m => HasUtxoDB (ReaderT AllDbVars m) where
  getUtxoDbVar  = asks allDBUtxo


readUtxoDb :: HasUtxoDB m => m LDB.DB
readUtxoDb = liftIO . readMVar =<< getUtxoDbVar