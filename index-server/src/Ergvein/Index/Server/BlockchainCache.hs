{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.BlockchainCache where 

import Conduit
import Conversion
import Data.Default
import Data.Flat
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Base as  LDB.Base
import System.Directory

import qualified Data.ByteString as B
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as Map

import Ergvein.Index.Server.BlockScanner.Types
import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Schema
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

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
cacheTxInfos db infos = write db def $ putItems (flat . TxCacheRecKey . tx'hash) (convert @TxInfo @TxCacheRec) infos

cacheTxInInfos :: MonadIO m => DB -> [TxInInfo] -> m ()
cacheTxInInfos db infos = write db def $ putItems (\info -> flat $ TxInCacheRecKey (txIn'txOutHash info) $ txIn'txOutIndex info) txIn'txHash infos

cacheTxOutInfos :: MonadIO m => DB -> [TxOutInfo] -> m ()
cacheTxOutInfos db infos = do
  let updateMap = fmap (convert @TxOutInfo @TxOutCacheRecItem) <$> (groupMapBy txOut'pubKeyScriptHash infos)
  cached <- sequence $ getCached <$> (Map.keys updateMap :: [PubKeyScriptHash])
  let cachedMap = Map.fromList $ catMaybes cached
      updated = Map.toList $ Map.unionWith (++) cachedMap updateMap
  write db def $ putItems (flat . TxOutCacheRecKey . fst) snd updated
  where
    getCached pubScriptHash = do
      maybeStored <- get db def $ flat $ TxOutCacheRecKey pubScriptHash
      let parsedMaybe = unflatExact <$> maybeStored
      pure $ (pubScriptHash,) <$> parsedMaybe

addToCache :: MonadIO m => DB -> BlockInfo -> m ()
addToCache db update = do
  cacheTxOutInfos db $ blockContent'TxOutInfos $ blockInfo'content update
  cacheTxInInfos db $ blockContent'TxInInfos $ blockInfo'content update
  cacheTxInfos db $ blockContent'TxInfos $ blockInfo'content update

openDb :: MonadIO m => m DB
openDb = do
  dbDirectory <- liftIO $ levelDbDir
  db <- LDB.Base.open dbDirectory def {createIfMissing = True }
  pure db

loadCache :: DB -> DBPool -> IO ()
loadCache db pool = do
  dbDirectory <- levelDbDir
  isDbDirExist <- liftIO $ doesDirectoryExist dbDirectory
  if isDbDirExist then removeDirectoryRecursive dbDirectory else pure ()
  
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