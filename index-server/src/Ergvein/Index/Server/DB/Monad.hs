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
  getUtxoDb :: m LevelDB

class MonadIO m => HasLMDBs m where
  getDb :: DBTag -> m LevelDB

data DBTag = DBFilters | DBIndexer | DBUtxo
  deriving (Eq, Show)

class HasBtcRollback m where
  getBtcRollbackVar :: m (TVar (Seq.Seq RollbackRecItem))

instance MonadIO m => HasIndexerDB (ReaderT LevelDB m) where
  getIndexerDb = ask

instance MonadIO m => HasFiltersDB (ReaderT LevelDB m) where
  getFiltersDb  = ask

instance MonadIO m => HasUtxoDB (ReaderT LevelDB m) where
  getUtxoDb = ask

data AllDbVars = AllDbVars {
  allDBFilters :: LevelDB
, allDBIndexer :: LevelDB
, allDBUtxo    :: LevelDB
}

instance MonadIO m => HasIndexerDB (ReaderT AllDbVars m) where
  getIndexerDb = asks allDBFilters

instance MonadIO m => HasFiltersDB (ReaderT AllDbVars m) where
  getFiltersDb  = asks allDBIndexer

instance MonadIO m => HasUtxoDB (ReaderT AllDbVars m) where
  getUtxoDb  = asks allDBUtxo
