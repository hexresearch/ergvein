{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.Cache where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Catch
import Conversion
import Data.Default
import Data.Flat
import Data.List
import Data.Maybe
import Data.Proxy
import Data.Text (Text, pack)
import Data.Word
import Database.LevelDB.Base
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

import qualified Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map

instance Conversion TxOutInfo TxOutCacheRecItem where
  convert txOutInfo = TxOutCacheRecItem (txOutIndex txOutInfo) (txOutValue txOutInfo) (txOutTxHash txOutInfo)

instance Conversion TxInfo TxCacheRec where
  convert txInfo = TxCacheRec (txHash txInfo) (txHexView txInfo) (txBlockHeight txInfo) (txBlockIndex txInfo)

cacheBlockMetaInfos :: MonadIO m => DB -> [BlockMetaInfo] -> m ()
cacheBlockMetaInfos db infos = write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = cachedMetaKey (blockMetaCurrency info, blockMetaBlockHeight info)
    valueSelector info = BlockMetaCacheRec (blockMetaHeaderHexView info) (blockMetaAddressFilterHexView info)

cacheTxInfos :: MonadIO m => DB -> [TxInfo] -> m ()
cacheTxInfos db infos = do
  write db def $ putItems (cachedTxKey . txHash) (convert @TxInfo @TxCacheRec) infos

cacheTxInInfos :: MonadIO m => DB -> [TxInInfo] -> m ()
cacheTxInInfos db infos = write db def $ putItems (\info -> cachedTxInKey (txInTxOutHash info, txInTxOutIndex info)) txInTxHash infos

cacheTxOutInfos :: MonadIO m => DB -> [TxOutInfo] -> m ()
cacheTxOutInfos db infos = do
  let updateMap = fmap (convert @TxOutInfo @TxOutCacheRecItem) <$> (groupMapBy txOutPubKeyScriptHash infos)
  cached <- mapM getCached $ Map.keys updateMap
  let cachedMap = Map.fromList $ catMaybes cached
      updated = Map.toList $ Map.unionWith (++) cachedMap updateMap
  write db def $ putItems (cachedTxOutKey . fst) snd updated
  where
    getCached pubScriptHash = do
      maybeStored <- get db def $ cachedTxOutKey pubScriptHash
      let parsedMaybe = unflatExact <$> maybeStored
      pure $ (pubScriptHash,) <$> parsedMaybe

addToCache :: MonadIO m => DB -> BlockInfo -> m ()
addToCache db update = do
  cacheTxOutInfos db $ blockContentTxOutInfos $ blockInfoContent update
  cacheTxInInfos db $ blockContentTxInInfos $ blockInfoContent update
  cacheTxInfos db $ blockContentTxInfos $ blockInfoContent update
  cacheBlockMetaInfos db $ [blockInfoMeta update]

openCacheDb :: (MonadLogger m, MonadIO m) => FilePath -> DBPool -> m DB
openCacheDb cacheDirectory pool = do
  canonicalPathDirectory <- liftIO $ canonicalizePath cacheDirectory
  isDbDirExist <- liftIO $ doesDirectoryExist canonicalPathDirectory
  liftIO $ if isDbDirExist then pure ()
                           else createDirectory canonicalPathDirectory                  
  levelDBContext <- liftIO $ open canonicalPathDirectory def `catch` restoreCache pool canonicalPathDirectory
  pure levelDBContext
  where
    clearDirectoryContent path = do
      content <- listDirectory path
      let contentFullPaths = (path </>) <$> content
      forM_ contentFullPaths removePathForcibly

    restoreCache :: DBPool -> FilePath -> SomeException -> IO DB
    restoreCache p pt _ = do
       clearDirectoryContent pt
       ctx <- open pt def {createIfMissing = True }
       runStdoutLoggingT $ loadCache ctx p
       pure ctx

loadCache :: (MonadLogger m, MonadIO m) => DB -> DBPool -> m ()
loadCache db pool = do
  logInfoN "Loading cache"

  txOutChunksCount <- runDbQuery pool (chunksCount (Proxy :: Proxy TxOutRec))
  runDbQuery pool $ runConduit 
     $ DCI.zipSources (chunksEnumeration txOutChunksCount) (pagedEntitiesStream TxOutRecId)
    .| CL.mapM  (logLoadingProgress "outputs" txOutChunksCount)
    .| CL.mapM_ (cacheTxOutInfos db . fmap convert)
    .| sinkList

  txInChunksCount <- runDbQuery pool (chunksCount (Proxy :: Proxy TxInRec))
  runDbQuery pool $ runConduit
     $ DCI.zipSources (chunksEnumeration txInChunksCount) (pagedEntitiesStream TxInRecId)
    .| CL.mapM  (logLoadingProgress "inputs" txInChunksCount)
    .| CL.mapM_ (cacheTxInInfos db . fmap convert)
    .| sinkList

  txChunksCount <- runDbQuery pool (chunksCount (Proxy :: Proxy TxRec))
  runDbQuery pool $ runConduit
     $ DCI.zipSources (chunksEnumeration txChunksCount) (pagedEntitiesStream TxRecId)
    .| CL.mapM  (logLoadingProgress "transactions" txChunksCount)
    .| CL.mapM_ (cacheTxInfos db . fmap convert)
    .| sinkList

  blockMetaChunksCount <- runDbQuery pool (chunksCount (Proxy :: Proxy BlockMetaRec))
  runDbQuery pool $ runConduit
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