{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ergvein.Index.Server.BlockchainCache where 

import Ergvein.Types.Transaction
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Data.Monoid
import Control.Monad.IO.Unlift
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.DB.Queries
import Data.Function
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Database.Persist.Types
import Ergvein.Index.Server.BlockScanner.Types
import Conversion
import Control.DeepSeq
import GHC.Generics
import Ergvein.Types.Currency
import Data.Default
import Control.DeepSeq
import Control.Monad.Trans.Resource (release)
import Data.Flat
import qualified Data.ByteString         as B
import Data.Maybe
import Data.Either
import qualified Data.Text.IO as T
import Data.Text (Text, pack, unpack)
import Control.Monad.Writer
import Control.DeepSeq
import Database.Persist.Pagination
import qualified Data.Conduit.List as CL
import           Conduit
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
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

writeBatch :: (MonadIO m, Flat k, Flat v) =>  DB -> B.ByteString -> [(k, v)] -> m ()
writeBatch db keyPrefix items = write db def $ storedRepresentation <$> items
  where
    storedRepresentation (key, value) = Put (keyPrefix <> flat key) (flat value)

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
  cacheTxInfos db $ block'TxInfos update
  cacheTxOutInfos db $ block'TxOutInfos update
  cacheTxInInfos db $ block'TxInInfos update

fromPersisted :: DBPool -> IO BlockInfo
fromPersisted pool = do 
  --txInEntities <- runDbQuery pool getAllTxIn
  --txOutEntities <- runDbQuery pool getAllTxOut
  --txEntities <- runDbQuery pool getAllTx
  z <- runDbQuery pool $ runConduit $ pagedEntitiesStream TxOutRecId .| sinkList
  let x = convert $ BlockInfo [] [] (convert <$> z) 
  x `deepseq`  pure x

levelDbDir :: IO FilePath
levelDbDir = (++ "/ldbCache") <$> getCurrentDirectory