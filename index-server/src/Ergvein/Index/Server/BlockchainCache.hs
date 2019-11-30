{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.BlockchainCache where 

import Ergvein.Types.Transaction
import qualified Data.Map.Strict as Map
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.BlockScanner.Types
import Conversion
import Control.DeepSeq
import Ergvein.Types.Currency
import Data.Default
import Data.Flat
import qualified Data.ByteString as B
import Data.Maybe
import Data.Either
import qualified Data.Conduit.List as CL
import           Conduit
import System.Directory
import Database.LevelDB

data CachedTxOut = CachedTxOut
  { cachedTxOut'index  :: TxOutIndex
  , cachedTxOut'value  :: MoneyUnit
  , cachedTxOut'txHash :: TxHash
  } deriving (Generic, NFData, Flat, Show)

data CachedTx = CachedTx
  { cachedTx'hash         :: TxHash
  , cachedTx'blockHeight  :: BlockHeight
  , cachedTx'blockIndex   :: TxBlockIndex
  } deriving (Generic, NFData, Flat, Show)

instance Conversion TxOutInfo CachedTxOut where
  convert txOutInfo = CachedTxOut (txOut'index txOutInfo) (txOut'value txOutInfo) (txOut'txHash txOutInfo)

instance Conversion TxInfo CachedTx where
  convert txInfo = CachedTx (tx'hash txInfo) (tx'blockHeight txInfo) (tx'blockIndex txInfo)

cachedTxKey :: B.ByteString
cachedTxKey = "tx"

cachedTxOutKey :: B.ByteString
cachedTxOutKey = "out"

cachedTxInKey :: B.ByteString
cachedTxInKey = "in"

cachedMetaKey :: B.ByteString
cachedMetaKey = "meta"

groupMapBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupMapBy keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

mapBy :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapBy keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

get' :: (MonadIO m, Flat k, Flat v) => DB -> B.ByteString -> k -> m (Maybe (k, v))
get' db keyPrefix key = do 
  maybeResult <- get db def $ keyPrefix <> flat key
  let maybeParsedResult = fromRight parsingError . unflat <$> maybeResult
  pure $ (key,) <$> maybeParsedResult
  where
    parsingError = error "error parsing"

getInKeySpace :: (MonadIO m, Flat k) => DB -> B.ByteString -> k -> m (Maybe B.ByteString)
getInKeySpace db keyPrefix key = get db def $ keyPrefix <> flat key

unflatExact :: (Flat b) => B.ByteString -> b
unflatExact = fromRight (error "Flat parsing error") . unflat

writeBatch :: (MonadIO m, Flat k, Flat v) =>  DB -> B.ByteString -> [(k, v)] -> m ()
writeBatch db keyPrefix items = write db def $ storedRepresentation <$> items
  where
    storedRepresentation (key, value) = Put (keyPrefix <> flat key) (flat value)


cacheBlockMetaInfos :: MonadIO m => DB -> [BlockMetaInfo] -> m ()
cacheBlockMetaInfos db infos =  writeBatch db "" $ (\info -> ((blockMeta'currency info, blockMeta'blockHeight info), blockMeta'headerHexView info)) <$> infos

cacheTxInfos :: MonadIO m => DB -> [TxInfo] -> m ()
cacheTxInfos db infos = writeBatch db cachedTxKey $ (\info -> (tx'hash info, convert @TxInfo @CachedTx info)) <$> infos

cacheTxInInfos :: MonadIO m => DB -> [TxInInfo] -> m ()
cacheTxInInfos db infos = writeBatch db cachedTxInKey $ (\info -> ((txIn'txOutHash info, txIn'txOutIndex info), txIn'txHash info)) <$> infos

cacheTxOutInfos :: MonadIO m => DB -> [TxOutInfo] -> m ()
cacheTxOutInfos db infos = do
  let updateMap = fmap (convert @TxOutInfo @CachedTxOut) <$> (groupMapBy txOut'pubKeyScriptHash infos)
  cached <- sequence $ get' db cachedTxOutKey <$> (Map.keys updateMap :: [PubKeyScriptHash])
  let cachedMap = Map.fromList $ catMaybes cached     
      updated = Map.toList $ Map.unionWith (++) cachedMap updateMap
  writeBatch db cachedTxOutKey updated

addToCache :: MonadIO m => DB -> BlockInfo -> m ()
addToCache db update = do
  cacheTxOutInfos db $ blockContent'TxOutInfos $ blockInfo'content update
  cacheTxInInfos db $ blockContent'TxInInfos $ blockInfo'content update
  cacheTxInfos db $ blockContent'TxInfos $ blockInfo'content update

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