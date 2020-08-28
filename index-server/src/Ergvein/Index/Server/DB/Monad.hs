{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Monad where

import Control.Monad.IO.Unlift
import qualified Database.LevelDB as LDB

class  MonadIO m => HasFiltersDB m where
  getFiltersDb :: m LDB.DB

class MonadIO m => HasIndexerDB m where
  getIndexerDb :: m LDB.DB

class MonadIO m => HasLMDBs m where
  getDb :: DBTag -> m LDB.DB

data DBTag = DBFilters | DBIndexer
  deriving (Eq)
