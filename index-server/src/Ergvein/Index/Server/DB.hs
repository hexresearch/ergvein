{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.DB
  (
    DBTag(..)
  , openDb
  , withDb
  ) where

import Conduit
import Control.Monad.Catch
import Control.Monad
import Control.Monad.Logger
import Data.Default
import Database.LevelDB.Base
import System.Directory

import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Wrapper
import qualified Ergvein.Index.Server.DB.Schema.Filters as DBF
import qualified Ergvein.Index.Server.DB.Schema.Indexer as DBI
import qualified Ergvein.Index.Server.DB.Schema.Utxo    as DBU

data MyException = DbVersionMismatch
    deriving Show

instance Exception MyException

openDb :: (MonadLogger m, MonadIO m) => Bool -> DBTag -> FilePath -> m LevelDB
openDb overwriteDbVerOnMismatch dbtag dbDirectory = do
  canonicalPathDirectory <- liftIO $ canonicalizePath dbDirectory
  dbStatePresent <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ unless dbStatePresent $ createDirectory canonicalPathDirectory
  levelDBContext <- liftIO $ do
    db <- openLevelDB canonicalPathDirectory def {createIfMissing = True }
    if overwriteDbVerOnMismatch || not dbStatePresent then do
      putLDB db def schemaVersionRecKey schemaVersion
      pure db
    else do
      dbSchemaVersion <- getLDB db def schemaVersionRecKey
      if dbSchemaVersion == Just schemaVersion then
        pure db
      else do
        let (dbName, overrideFlag) = case dbtag of
              DBFilters -> ("Filters", "--override-ver-filters")
              DBIndexer -> ("Indexer", "--override-ver-indexer")
              DBUtxo    -> ("UTXO", "--override-ver-utxo")
        putStrLn $ "[" <> dbName <> "]: Error! Database version mismatch!"
        putStrLn $ "[" <> dbName <> "]: If you are sure, that the new schema is compatible, run with " <> overrideFlag
        throwM DbVersionMismatch
  pure levelDBContext
  where
    (schemaVersionRecKey, schemaVersion) = case dbtag of
      DBFilters -> (DBF.schemaVersionRecKey, DBF.schemaVersion)
      DBIndexer -> (DBI.schemaVersionRecKey, DBI.schemaVersion)
      DBUtxo    -> (DBU.schemaVersionRecKey, DBU.schemaVersion)
withDb
  :: (MonadLogger m, MonadIO m, MonadMask m)
  => Bool -> DBTag -> FilePath -> (LevelDB -> m a) -> m a
withDb overwriteDbVerOnMismatch dbtag dbDirectory =
  -- NOTE: Not quite ideal. It's still possible to lose handle if
  --       we're interrupted in openDb
  bracket
    (openDb overwriteDbVerOnMismatch dbtag dbDirectory)
    closeLevelDB
