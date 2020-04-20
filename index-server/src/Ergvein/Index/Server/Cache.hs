{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.Cache where

import Conduit
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Logger
import Conversion
import Data.Default
import Data.Flat
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Text (Text, pack)
import Data.Word
import Database.LevelDB.Base
import Database.LevelDB.Internal
import Database.Persist.Pagination.Types
import System.Directory
import System.FilePath

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Queries
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Utils
import Ergvein.Text
import Database.LevelDB.Internal
import Conversion

import qualified Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map



cacheBlockMetaInfos :: MonadIO m => DB -> [BlockMetaInfo] -> m ()
cacheBlockMetaInfos db infos = write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = cachedMetaKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaCacheRec (blockMetaHeaderHashHexView info) (blockMetaAddressFilterHexView info)

cacheTxInfos :: MonadIO m => DB -> [TxInfo] -> m ()
cacheTxInfos db infos = do
  write db def $ putItems (cachedTxKey . txHash) (convert @TxInfo @TxCacheRec) infos

addToCache :: (MonadLDB m) => BlockInfo -> m ()
addToCache update = do
  db <- getDb
  updateTxSpends (spentTxsHash update) $ blockContentTxInfos update
  cacheBlockMetaInfos db $ [blockInfoMeta update]

openCacheDb :: (MonadLogger m, MonadIO m) => FilePath -> DBPool -> m DB
openCacheDb cacheDirectory pool = do
  canonicalPathDirectory <- liftIO $ canonicalizePath cacheDirectory
  isDbDirExist <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ unless isDbDirExist $ createDirectory canonicalPathDirectory
  levelDBContext <- liftIO $ do
    db <- open canonicalPathDirectory def
    dbSchemaVersion <- dbSchemaVersion db
    if dbSchemaVersion == Just schemaVersion then do
      pure db
    else do
      unsafeClose db
      restoreCache canonicalPathDirectory
    `catch` (\(SomeException _) -> restoreCache canonicalPathDirectory)
  pure levelDBContext
  where
    dbSchemaVersion db = do
      maybeDbSchemaVersion <- get db def cachedSchemaVersionKey 
      pure $ unflatExact <$> maybeDbSchemaVersion
    clearDirectoryContent path = do
      content <- listDirectory path
      let contentFullPaths = (path </>) <$> content
      forM_ contentFullPaths removePathForcibly

    restoreCache :: FilePath -> IO DB
    restoreCache pt = do
       clearDirectoryContent pt
       ctx <- open pt def {createIfMissing = True }
       async $ runStdoutLoggingT $ loadCache ctx pool
       put ctx def cachedSchemaVersionKey (flat schemaVersion) 
       pure ctx

loadCache :: (MonadLogger m, MonadIO m) => DB -> DBPool -> m ()
loadCache db pool = do
  logInfoN "Loading cache"

  blockMetaChunksCount <- dbQueryManual pool (chunksCount (Proxy :: Proxy BlockMetaRec))
  dbQueryManual pool $ runConduit
     $ DCI.zipSources (chunksEnumeration blockMetaChunksCount) (pagedEntitiesStream BlockMetaRecId)
    .| CL.mapM  (logLoadingProgress "block meta" blockMetaChunksCount)
    .| CL.mapM_ (cacheBlockMetaInfos db . fmap convert)
    .| sinkList

  pure ()
  where 
    chunksEnumeration chunkCount = CL.enumFromTo 1 chunkCount
    logLoadingProgress :: MonadLogger m => Text -> Word64 -> (Word64, a) -> m a
    logLoadingProgress entityType chunkCount (chunkIndex, chunkData) = do
      logInfoN $ "Loading " <> entityType <> " cache: " <> showf 2 (fromIntegral chunkIndex / fromIntegral chunkCount * 100 :: Double) <> "%"
      pure chunkData