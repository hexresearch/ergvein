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

data BCCache = BCCache
  { cache'byScript :: Map.Map PubKeyScriptHash [ScriptHistoryCached]
  } deriving (Generic, NFData)

class MonadUnliftIO m => BCache m where
  getCache :: m (TVar BCCache)

instance Default BCCache where
  def = BCCache mempty

addToCache :: BCCache -> BlockInfo -> BCCache
addToCache cache update = let
  
  outsD =  sortAndGroup txOut'pubKeyScriptHash $ block'TxOutInfos update
  putsDM = fmap (\x-> Unspent $ CachedUnspentTxOut (txOut'index x) (txOut'value x) (txOut'txHash x)) <$> outsD

  newAggr =  Map.unionWith (++) putsDM $ cache'byScript cache
  newAggr' = fmap (fmap toCached) newAggr

  in force $ BCCache newAggr'
  where
    insD = Map.fromList $ (\x -> ((txIn'txOutHash x, txIn'txOutIndex x), x)) <$> block'TxInInfos update
    sortAndGroup selector assocs  = Map.fromListWith (++) $ (\x-> (selector x , [x])) <$> assocs
    toCached out = case out of
       Unspent o -> 
        case insD Map.!? (cachedUnspent'txHash o, cachedUnspent'index o) of
          Just s ->  Spent $ CachedSpentTxOut (txIn'txHash s)  (cachedUnspent'txHash o)
          otherwise -> out
       otherwise -> out

{-addToCache' update = do
  let get' x = runCreateLevelDB "/tmp/mydb" "txOuts" $ get $ flat (txIn'txOutHash x, txIn'txOutIndex x)
  runCreateLevelDB "/tmp/mydb" "txOuts" $ do
    let f x = putB (flat (txOut'txHash x, txOut'index x)) (flat $ (convert x ::TxOutCache))
    runBatch $ sequence_ $ f <$> block'TxOutInfos update
    pure ()
  r <- sequence $ f' <$> block'TxInInfos update
  let minuses = (fromRight $ error "") . (\x -> (unflat x) :: Decoded TxOutCache) . fromJust <$> r
  pure ()-}
addToCache' upd = runCreateLevelDB "/tmp/mydb" "txOuts" $ do
  let get' key = get $ flat key
      outsD =  sortAndGroup txOut'pubKeyScriptHash $ block'TxOutInfos upd
      putsDM = fmap (\x-> Unspent $ CachedUnspentTxOut (txOut'index x) (txOut'value x) (txOut'txHash x)) <$> outsD
      e m = case m of
        Just x -> x
        Nothing -> error "key absence"
  liftIO $ T.putStrLn $ pack $ "get"
  x <- sequence $ (\k -> (fmap (k,)) <$> get' k) <$> Map.keys outsD
  liftIO $  T.putStrLn $ pack $ "get end"
  let newAggr =  Map.unionWith (++) putsDM $ mempty-- Map.fromList $ (\r-> (fst r, fromRight (error "") $ unflat $ snd r)) <$> catMaybes x
      newAggr' = Map.toList $ fmap (fmap toCached) newAggr
      f (k,v) = putB (flat k)  (flat v)
  liftIO $  T.putStrLn $ pack $ "batch"
  runBatch $ sequence_ $ f <$> newAggr' 
  where
    insD = Map.fromList $ (\x -> ((txIn'txOutHash x, txIn'txOutIndex x), x)) <$> block'TxInInfos upd
    sortAndGroup selector assocs  = Map.fromListWith (++) $ (\x-> (selector x , [x])) <$> assocs
    toCached out = case out of
       Unspent o -> 
        case insD Map.!? (cachedUnspent'txHash o, cachedUnspent'index o) of
          Just s ->  Spent $ CachedSpentTxOut (txIn'txHash s)  (cachedUnspent'txHash o)
          otherwise -> out
       otherwise -> out

fromPersisted' :: DBPool -> IO BlockInfo
fromPersisted' pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx
  pure $ convert (txEntities, txInEntities, txOutEntities)

fromPersisted :: DBPool -> IO BCCache
fromPersisted pool = do 
  txInEntities <- runDbQuery pool getAllTxIn
  txOutEntities <- runDbQuery pool getAllTxOut
  txEntities <- runDbQuery pool getAllTx

  let z x = flat (txOut'txHash x, txOut'index x)
      blockInfo =  convert (txEntities, txInEntities, txOutEntities) :: BlockInfo
      cache = addToCache def blockInfo :: BCCache
  pure cache