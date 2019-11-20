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
import Data.Text
import Data.Maybe
import Data.Either
import Database.LevelDB.Higher
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import Control.Monad.Writer
import Control.DeepSeq

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

batchInsert :: MonadLevelDB m => [Item] -> WriterT WriteBatch m () 
batchInsert items = sequence_ $ uncurry putB <$> items

batchGet :: MonadLevelDB m => [Database.LevelDB.Higher.Key] -> m [Maybe Value]
batchGet keys = sequence $ get <$> keys

cachedHistory :: MonadLevelDB m => PubKeyScriptHash -> m (Maybe (PubKeyScriptHash, [ScriptHistoryCached]))
cachedHistory pubKeyScriptHash = do 
  maybeResult <- get $ flat pubKeyScriptHash
  let maybeParsedResult = fromRight parsingError . unflat <$> maybeResult
  pure $ (pubKeyScriptHash,) <$> maybeParsedResult
  where
    parsingError = error ("error parsing " ++ unpack pubKeyScriptHash)

toItem :: (Flat k, Flat v) => (s -> k) -> (s -> v) -> s -> Item
toItem keySelector valueSelector source = (flat $ keySelector source , flat $ valueSelector source)

addToCache:: MonadLevelDB m => BlockInfo -> m ()
addToCache update = do
  withKeySpace "txs" $ runBatch $ batchInsert $ toItem tx'hash (convert @TxInfo @CachedTx) <$> block'TxInfos update

  let updateHistoryMap = fmap convert <$> (listToGroupMap txOut'pubKeyScriptHash $ block'TxOutInfos update)

  cachedHistory <- sequence $ cachedHistory <$> Map.keys updateHistoryMap

  let cachedHistoryMap = Map.fromList $ catMaybes cachedHistory
      upsertHistoryMap =  Map.unionWith (++) updateHistoryMap cachedHistoryMap
      unspentUpdatedHistory =  Map.toList $ (fmap unspentUpdated) <$> upsertHistoryMap

  runBatch $ batchInsert $ toItem fst snd <$> unspentUpdatedHistory
  pure ()
  where
    txInsMap = listToMap (\x -> (txIn'txOutHash x, txIn'txOutIndex x)) $ block'TxInInfos update  
    unspentUpdated (Unspent out) =
      case txInsMap Map.!? (cachedUnspent'txHash out, cachedUnspent'index out) of
        Just spentInfo -> Spent $ CachedSpentTxOut (txIn'txHash spentInfo) $ cachedUnspent'txHash out
        otherwise -> Unspent out
    unspentUpdated (Spent out) = Spent out

deleteHistory :: MonadLevelDB m => m ()
deleteHistory = do 
  keys <- scan mempty queryBegins
    { scanInit = []
    , scanMap = \(key, value) -> key
    , scanFilter = const True
    , scanFold = (:)
    }
  runBatch $ sequence_ $ deleteB <$> keys
  pure ()

fromPersisted :: DBPool -> IO BlockInfo
fromPersisted pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx
  pure $ convert (txEntities, txInEntities, txOutEntities)