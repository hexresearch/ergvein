{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.DB
  (
    DBTag(..)
  , openDb
  ) where

import Conduit
import Control.Exception
import Control.Monad
import Control.Monad.Logger
import Data.Default
import Database.LevelDB.Base
import System.Directory

import Ergvein.Index.Server.DB.Monad

import qualified Ergvein.Index.Server.DB.Schema.Filters as DBF
import qualified Ergvein.Index.Server.DB.Schema.Indexer as DBI

data MyException = DbVersionMismatch
    deriving Show

instance Exception MyException

openDb :: (MonadLogger m, MonadIO m) => Bool -> DBTag -> FilePath -> m DB
openDb overwriteDbVerOnMismatch dbtag dbDirectory = do
  canonicalPathDirectory <- liftIO $ canonicalizePath dbDirectory
  dbStatePresent <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ unless dbStatePresent $ createDirectory canonicalPathDirectory
  levelDBContext <- liftIO $ do
    db <- open canonicalPathDirectory def {createIfMissing = True }
    if overwriteDbVerOnMismatch || not dbStatePresent then do
      put db def schemaVersionRecKey schemaVersion
      pure db
    else do
      dbSchemaVersion <- get db def schemaVersionRecKey
      if dbSchemaVersion == Just schemaVersion then
        pure db
      else do
        let (dbName, overrideFlag) = case dbtag of
              DBFilters -> ("Filters", "--override-ver-filters")
              DBIndexer -> ("Indexer", "--override-ver-indexer")
        putStrLn $ "[" <> dbName <> "]: Error! Database version mismatch!"
        putStrLn $ "[" <> dbName <> "]: If you are sure, that the new schema is compatible, run with " <> overrideFlag
        throw DbVersionMismatch
  pure levelDBContext
  where
    (schemaVersionRecKey, schemaVersion) = case dbtag of
      DBFilters -> (DBF.schemaVersionRecKey, DBF.schemaVersion)
      DBIndexer -> (DBI.schemaVersionRecKey, DBI.schemaVersion)
