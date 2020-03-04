{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.Cache where

import Conduit
import Control.Monad.Logger
import Control.Monad
import Conversion
import Data.Default
import Data.Flat
import Data.List
import Data.Maybe
import Database.LevelDB.Base
import System.Directory
import System.FilePath
import Data.Text (Text, pack)

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Queries
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.Utils
import           Data.Proxy
import qualified Data.Conduit.Internal as DCI
import Database.Persist.Pagination.Types

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

openCacheDb :: FilePath -> IO DB
openCacheDb cacheDirectory = do
  canonicalPathDirectory <- canonicalizePath cacheDirectory
  isDbDirExist <- doesDirectoryExist canonicalPathDirectory
  if isDbDirExist then clearDirectoryContent canonicalPathDirectory
                  else createDirectory canonicalPathDirectory

  open canonicalPathDirectory def {createIfMissing = True }
  where
    clearDirectoryContent path = do
      content <- listDirectory path
      let contentFullPaths = (path </>) <$> content
      forM_ contentFullPaths removePathForcibly
    

loadCache :: (MonadLogger m, MonadIO m) => DB -> DBPool -> m ()
loadCache db pool = do
  logInfoN "Loading cache"



  logInfoN "Loading outputs"
  runDbQuery pool $ runConduit $ pagedEntitiesStream TxOutRecId
    .| CL.mapM_ (cacheTxOutInfos db . fmap convert)
    .| sinkList

  logInfoN "Loading inputs"
  runDbQuery pool $ runConduit $ pagedEntitiesStream TxInRecId
    .| CL.mapM_ (cacheTxInInfos db . fmap convert)
    .| sinkList

  logInfoN "Loading transactions"
  runDbQuery pool $ runConduit $ pagedEntitiesStream TxRecId
    .| CL.mapM_ (cacheTxInfos db . fmap convert)
    .| sinkList

  logInfoN "Loading block headers"


  r <- runDbQuery pool (rowsCount (Proxy :: Proxy BlockMetaRec))
  error $ show r
  let cnt = chunkCount (unPageSize pageLoadSize) r

  runDbQuery pool $ runConduit $ 
     (DCI.zipSources (chunksEnumeration cnt) (pagedEntitiesStream BlockMetaRecId))
    .| CL.mapM (logLoadingProgress "block meta" cnt)
    .| CL.mapM_ (cacheBlockMetaInfos db . fmap convert)
    .| sinkList

  pure ()

logLoadingProgress :: MonadLogger m => Text -> Int -> (Int, a) -> m a
logLoadingProgress entityType chunkCount (chunkIndex, chunkData) = do
  logInfoN $ "Loading " <> entityType <> ": " <> (pack $ show chunkIndex) <> " of " <> (pack $ show chunkCount)
  pure chunkData

chunkCount chunkSize dataLength = ceiling $ fromIntegral dataLength / fromIntegral chunkSize

chunksEnumeration chunkCount= CL.enumFromTo 1 chunkCount