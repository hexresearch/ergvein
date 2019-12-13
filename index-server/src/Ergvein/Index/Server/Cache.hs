{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.Cache where 

import Conduit
import Conversion
import Data.Default
import Data.Flat
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Base as  LDB.Base
import System.Directory

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Types.Transaction

import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map

instance Conversion TxOutInfo TxOutCacheRecItem where
  convert txOutInfo = TxOutCacheRecItem (txOut'index txOutInfo) (txOut'value txOutInfo) (txOut'txHash txOutInfo)

instance Conversion TxInfo TxCacheRec where
  convert txInfo = TxCacheRec (tx'hash txInfo) (tx'blockHeight txInfo) (tx'blockIndex txInfo)

groupMapBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupMapBy keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

mapBy :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapBy keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

cacheBlockMetaInfos :: MonadIO m => DB -> [BlockMetaInfo] -> m ()
cacheBlockMetaInfos db infos = write db def $ putItems keySelector valueSelector infos
  where
    keySelector   info = cachedMetaKey (blockMeta'currency info, blockMeta'blockHeight info)
    valueSelector info = BlockMetaCacheRec $ blockMeta'headerHexView info

cacheTxInfos :: MonadIO m => DB -> [TxInfo] -> m ()
cacheTxInfos db infos = do 
  write db def $ putItems (cachedTxKey . tx'hash) (convert @TxInfo @TxCacheRec) infos

cacheTxInInfos :: MonadIO m => DB -> [TxInInfo] -> m ()
cacheTxInInfos db infos = write db def $ putItems (\info -> cachedTxInKey (txIn'txOutHash info, txIn'txOutIndex info)) txIn'txHash infos

cacheTxOutInfos :: MonadIO m => DB -> [TxOutInfo] -> m ()
cacheTxOutInfos db infos = do
  let updateMap = fmap (convert @TxOutInfo @TxOutCacheRecItem) <$> (groupMapBy txOut'pubKeyScriptHash infos)
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
  cacheTxOutInfos db $ blockContent'TxOutInfos $ blockInfo'content update
  cacheTxInInfos db $ blockContent'TxInInfos $ blockInfo'content update
  cacheTxInfos db $ blockContent'TxInfos $ blockInfo'content update
  cacheBlockMetaInfos db $ [blockInfo'meta update]

openDb :: IO DB
openDb = do
  dbDirectory <- levelDbDir
  isDbDirExist <- liftIO $ doesDirectoryExist dbDirectory
  if isDbDirExist then removeDirectoryRecursive dbDirectory else pure ()
  db <- LDB.Base.open dbDirectory def {createIfMissing = True }
  pure db

loadCache :: DB -> DBPool -> IO ()
loadCache db pool = do
  runDbQuery pool $ runConduit $ pagedEntitiesStream TxOutRecId 
    .| CL.mapM_ (cacheTxOutInfos db . fmap convert)
    .| sinkList

  runDbQuery pool $ runConduit $ pagedEntitiesStream TxInRecId 
    .| CL.mapM_ (cacheTxInInfos db . fmap convert)
    .| sinkList

  runDbQuery pool $ runConduit $ pagedEntitiesStream TxRecId 
    .| CL.mapM_ (cacheTxInfos db . fmap convert)
    .| sinkList

  runDbQuery pool $ runConduit $ pagedEntitiesStream BlockMetaRecId 
    .| CL.mapM_ (cacheBlockMetaInfos db . fmap convert)
    .| sinkList

  pure ()

levelDbDir :: IO FilePath
levelDbDir = (++ "/ldbCache") <$> getCurrentDirectory