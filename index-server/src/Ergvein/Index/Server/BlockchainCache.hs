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

data CachedUnspentTxOut = CachedUnspentTxOut
  { cachedUnspent'index  :: TxOutIndex
  , cachedUnspent'value  :: MoneyUnit
  , cachedUnspent'txHash :: TxHash
  }  deriving (Generic, NFData, Flat)

data CachedSpentTxOut = CachedSpentTxOut
  { cachedSpent'spentInTxHash  :: TxHash
  , cachedSpent'txHash         :: TxHash
  }  deriving (Generic, NFData, Flat)

data ScriptHistoryCached = Unspent CachedUnspentTxOut | Spent CachedSpentTxOut  deriving (Generic, NFData, Flat)

instance Conversion TxOutInfo ScriptHistoryCached where
  convert txOutInfo = Unspent $ CachedUnspentTxOut (txOut'index txOutInfo) (txOut'value txOutInfo) (txOut'txHash txOutInfo) 

listToGroupMap :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
listToGroupMap keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

listToMap :: Ord k => (v -> k) -> [v] -> Map.Map k v
listToMap keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

cachedHistory :: MonadLevelDB m => PubKeyScriptHash -> m (PubKeyScriptHash, [ScriptHistoryCached])
cachedHistory pubKeyScriptHash = do 
  maybeResult <- get $ flat pubKeyScriptHash
  let result = fromMaybe (error "not found") $ maybeResult
      parsedResult = fromRight (error "parse error") $ unflat result
  pure parsedResult

upsertHistory :: MonadLevelDB m => (PubKeyScriptHash, [ScriptHistoryCached]) -> WriterT WriteBatch m () 
upsertHistory (key, value) = putB (flat key) $ flat value

runCreateLevelDB' :: (MonadThrow m, MonadUnliftIO m)
           => LevelDBT m a -- ^ The actions to execute
           -> m a

runCreateLevelDB'  = runCreateLevelDB "/tmp/mydb" "txOuts"

--x = runCreateLevelDB "/tmp/mydb" "txOuts"

addToCache :: MonadLevelDB m => BlockInfo -> m ()
addToCache update = do
  let updateHistoryMap = fmap convert <$> (listToGroupMap txOut'pubKeyScriptHash $ block'TxOutInfos update)

  cachedHistory <- sequence $ cachedHistory <$> Map.keys updateHistoryMap

  let cachedHistoryMap = Map.fromList cachedHistory
      upsertHistoryMap =  Map.unionWith (++) updateHistoryMap cachedHistoryMap
      unspentUpdatedHistory =  Map.toList $ (fmap unspentUpdated) <$> upsertHistoryMap

  runBatch $ sequence_ $ upsertHistory <$> unspentUpdatedHistory
  where
    txInsMap = listToMap (\x -> (txIn'txOutHash x, txIn'txOutIndex x)) $ block'TxInInfos update  
    unspentUpdated (Unspent out) =
      case txInsMap Map.!? (cachedUnspent'txHash out, cachedUnspent'index out) of
        Just spentInfo -> Spent $ CachedSpentTxOut (txIn'txHash spentInfo) $ cachedUnspent'txHash out
        otherwise -> Unspent out
    unspentUpdated (Spent out) = Spent out

fromPersisted :: DBPool -> IO BlockInfo
fromPersisted pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx
  pure $ convert (txEntities, txInEntities, txOutEntities)