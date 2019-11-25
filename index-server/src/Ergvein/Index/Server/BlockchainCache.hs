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

data CachedUnspentTxOut = CachedUnspentTxOut
  { cachedUnspent'index  :: TxOutIndex
  , cachedUnspent'value  :: MoneyUnit
  , cachedUnspent'txHash :: TxHash
  }  deriving (Generic, NFData, Flat, Show)

data CachedSpentTxOut = CachedSpentTxOut
  { cachedSpent'spentInTxHash  :: TxHash
  , cachedSpent'txHash         :: TxHash
  }  deriving (Generic, NFData, Flat, Show)

data CachedTx = CachedTx
  { cachedTx'hash :: TxHash
  , cachedTx'blockHeight  :: BlockHeight
  , cachedTx'blockIndex   :: TxBlockIndex
  }  deriving (Generic, NFData, Flat, Show)

data ScriptHistoryCached = Unspent CachedUnspentTxOut | Spent CachedSpentTxOut  deriving (Generic, NFData, Flat, Show)

instance Conversion TxOutInfo ScriptHistoryCached where
  convert txOutInfo = Unspent $ CachedUnspentTxOut (txOut'index txOutInfo) (txOut'value txOutInfo) (txOut'txHash txOutInfo)

instance Conversion TxInfo CachedTx where
  convert txInfo = CachedTx (tx'hash txInfo) (tx'blockHeight txInfo) (tx'blockIndex txInfo)

listToGroupMap :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
listToGroupMap keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

listToMap :: Ord k => (v -> k) -> [v] -> Map.Map k v
listToMap keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

cachedHistory :: MonadIO m => DB -> PubKeyScriptHash -> m (Maybe (PubKeyScriptHash, [ScriptHistoryCached]))
cachedHistory db pubKeyScriptHash = do 
  maybeResult <- get db def $ historyKey pubKeyScriptHash
  let maybeParsedResult = fromRight parsingError . unflat <$> maybeResult
  pure $ (pubKeyScriptHash,) <$> maybeParsedResult
  where
    parsingError = error ("error parsing " ++ unpack pubKeyScriptHash)

toItem :: (Flat k, Flat v) => (s -> k) -> (s -> v) -> s -> (B.ByteString, B.ByteString)
toItem keySelector valueSelector source = (flat $ keySelector source , flat $ valueSelector source)

historyKey :: PubKeyScriptHash -> B.ByteString
historyKey pubKeyScriptHash = "hst" <> flat pubKeyScriptHash

txKey :: PubKeyScriptHash -> B.ByteString
txKey pubKeyScriptHash = "hst" <> flat pubKeyScriptHash

addToCache :: MonadIO m => DB -> BlockInfo -> m ()
addToCache db update = do
  write db def $ (\x -> Put (txKey $ tx'hash x) $ flat $ convert @TxInfo @CachedTx x) <$> block'TxInfos update

  let updateHistoryMap = fmap convert <$> (listToGroupMap txOut'pubKeyScriptHash $ block'TxOutInfos update)

  cachedHistory <- sequence $ cachedHistory db <$> Map.keys updateHistoryMap

  let cachedHistoryMap = Map.fromList $ catMaybes cachedHistory
      upsertHistoryMap =  Map.unionWith (++) updateHistoryMap cachedHistoryMap
      unspentUpdatedHistory =  Map.toList $ (fmap unspentUpdated) <$> upsertHistoryMap

  write db def $ (\x -> Put (historyKey $ fst x) $ flat $ snd x) <$> unspentUpdatedHistory
  pure ()
  where
    txInsMap = listToMap (\x -> (txIn'txOutHash x, txIn'txOutIndex x)) $ block'TxInInfos update  
    unspentUpdated (Unspent out) =
      case txInsMap Map.!? (cachedUnspent'txHash out, cachedUnspent'index out) of
        Just spentInfo -> Spent $ CachedSpentTxOut (txIn'txHash spentInfo) $ cachedUnspent'txHash out
        otherwise -> Unspent out
    unspentUpdated (Spent out) = Spent out


loadCache :: DB -> DBPool -> IO ()
loadCache db dbPool = do 
  persisted <- fromPersisted dbPool
  T.putStrLn $ pack $ "from db done"
  persisted `deepseq` addToCache db persisted

fromPersisted :: DBPool -> IO BlockInfo
fromPersisted pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx
  pure $ force $ convert (txEntities, txInEntities, txOutEntities)

levelDbDir :: IO FilePath
levelDbDir = (++ "/tmp/ldb") <$> getCurrentDirectory