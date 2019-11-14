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

data TxOutCache = TxOutCache
  { txCachedOut'pubKeyScriptHash :: PubKeyScriptHash
  , txCachedOut'value   :: MoneyUnit
  } deriving (Generic, NFData)

instance Conversion TxOutInfo TxOutCache where
  convert out = TxOutCache (txOut'pubKeyScriptHash out) (txOut'value out)

data BCCache = BCCache
  { cache'byScript :: Map.Map PubKeyScriptHash MoneyUnit
  , cache'txOuts  :: Map.Map (TxHash, TxOutIndex) TxOutCache
  } deriving (Generic, NFData)

class MonadUnliftIO m => BCache m where
  getCache :: m (TVar BCCache)

instance Default BCCache where
  def = BCCache mempty mempty

addToCache :: BCCache -> BlockInfo -> BCCache
addToCache cache update = let
  newOutsD = outsS `Map.union` cache'txOuts cache
  ins = Map.fromListWith (+)  $ (\x-> (txCachedOut'pubKeyScriptHash x, (- txCachedOut'value x)))
                             <$> (\x -> outsS Map.! (txIn'txOutHash x, txIn'txOutIndex x))
                             <$> block'TxInInfos update
  outs = Map.fromListWith (+) $ (\x-> (txOut'pubKeyScriptHash x, txCachedOut'value $ convert x))
                             <$> block'TxOutInfos update
  delta = Map.unionWith (+) ins outs
  newAggr = Map.unionWith (+) delta $ cache'byScript cache

  in force $ BCCache newAggr newOutsD
  where
    sortAndGroup assocs selector = Map.fromListWith (++) $ (\x-> (selector x , [x])) <$> assocs
    outsS = Map.fromList $ (\x' -> ((txOut'txHash x', txOut'index x'), convert x')) <$> block'TxOutInfos update

fromPersisted :: DBPool -> IO BCCache
fromPersisted pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx
  let blockInfo =  convert (txEntities, txInEntities, txOutEntities) :: BlockInfo
      cache = addToCache def blockInfo :: BCCache
  pure cache