{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.DB
  (
    DBTag(..)
  , openDb
  ) where

import Conduit
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Conversion
import Data.ByteString
import Data.Default
import Data.Flat
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Text (Text, pack)
import Data.Word
import Database.LevelDB.Base
import Database.LevelDB.Internal
import System.Directory
import System.FilePath

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries (initIndexerDb)
import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.Utils
import Ergvein.Text

import qualified Ergvein.Index.Server.DB.Schema.Filters as DBF
import qualified Ergvein.Index.Server.DB.Schema.Indexer as DBI
import qualified Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map

openDb :: (MonadLogger m, MonadIO m) => Bool -> DBTag -> FilePath -> m DB
openDb noDropFilters dbtag dbDirectory = do
  canonicalPathDirectory <- liftIO $ canonicalizePath dbDirectory
  isDbDirExist <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ unless isDbDirExist $ createDirectory canonicalPathDirectory
  levelDBContext <- liftIO $ do
    db <- open canonicalPathDirectory def {createIfMissing = True }
    if (noDropFilters && dbtag == DBFilters) then do
      put db def schemaVersionRecKey (flat schemaVersion)
      pure db
      else do
        dbSchemaVersion <- dbSchemaVersion db
        if dbSchemaVersion == Just schemaVersion then pure db
          else do
            unsafeClose db
            restoreDb canonicalPathDirectory
    `catch` (\(SomeException _) -> restoreDb canonicalPathDirectory)
  pure levelDBContext
  where
    (schemaVersionRecKey, schemaVersion) = case dbtag of
      DBFilters -> (DBF.schemaVersionRecKey, DBF.schemaVersion)
      DBIndexer -> (DBI.schemaVersionRecKey, DBI.schemaVersion)
    dbSchemaVersion db = do
      maybeDbSchemaVersion <- get db def schemaVersionRecKey
      pure $ unflatExact <$> maybeDbSchemaVersion
    clearDirectoryContent path = do
      content <- listDirectory path
      let contentFullPaths = (path </>) <$> content
      forM_ contentFullPaths removePathForcibly

    restoreDb :: FilePath -> IO DB
    restoreDb pt = do
       clearDirectoryContent pt
       ctx <- open pt def {createIfMissing = True }
       put ctx def schemaVersionRecKey (flat schemaVersion)
       when (dbtag == DBIndexer) $ initIndexerDb ctx
       pure ctx
