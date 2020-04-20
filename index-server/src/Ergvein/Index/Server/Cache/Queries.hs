module Ergvein.Index.Server.Cache.Queries where

import Control.Lens
import Data.Default
import Data.Flat
import Data.Maybe
import Database.LevelDB
import Database.LevelDB.Iterator

import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Types.Transaction
import Ergvein.Index.Server.BlockchainScanning.Types
import Conversion

import Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Data.Map.Strict as Map
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming
import Debug.Trace
import Control.Monad.IO.Class

instance Conversion TxInfo TxCacheRec where
  convert txInfo = TxCacheRec (txHash txInfo) (txHexView txInfo) (txOutputsCount txInfo)

safeEntrySlice :: (MonadLDB m , Ord k, S.Serialize k, Flat v) => BS.ByteString -> k -> m [(k,v)]
safeEntrySlice startKey endKey = do
  db <- getDb
  iterator <- createIter db def
  slice <- LDBStreaming.toList $ LDBStreaming.entrySlice iterator range LDBStreaming.Asc
  pure $ over _2 unflatExact  <$> over _1 (decodeExact . unPrefixedKey) <$> slice
  where
    range = LDBStreaming.KeyRange startKey comparison
    comparison key = case S.decode $ unPrefixedKey key of
      Right parsedKey -> compare parsedKey endKey
      _ -> GT

getParsed :: (MonadLDB m, Flat v) => BS.ByteString -> m (Maybe v)
getParsed key = do
  db <- getDb
  maybeResult <- get db def key
  let maybeParsedResult = unflatExact <$> maybeResult
  pure maybeParsedResult

getParsedExact :: (MonadLDB m, Flat v) => BS.ByteString -> m v
getParsedExact key = do
  db <- getDb
  maybeResult <- get db def key
  let result = fromMaybe notFoundErr maybeResult
      parsedResult = unflatExact result
  pure parsedResult
  where
    notFoundErr = error $ "getParsedExact: not found" ++ show key

getManyParsedExact :: (MonadLDB m, Flat v) => [BS.ByteString] -> m [v]
getManyParsedExact keys = do
  db <- getDb
  result <- mapM getParsedExact keys
  pure result

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

updateTxSpends  :: (MonadLDB m) => [TxHash] -> [TxInfo] -> m ()
updateTxSpends spentTxsHash newTxInfos = do
  db <- getDb
  write db def $ putItems (cachedTxKey . txHash) (convert @_ @TxCacheRec) newTxInfos
  cachedInfo <- getManyParsedExact @_ @TxCacheRec  $ cachedTxKey <$> Map.keys outSpendsAmountByTx 
  write db def $ infoUpdate <$> cachedInfo
  where
    outSpendsAmountByTx = Map.fromListWith (+) $ (,1) <$> spentTxsHash
    infoUpdate info = let 
      outputsLeft = txCacheRecUnspentOutputsCount info - outSpendsAmountByTx Map.! (txCacheRecHash info)
      in if outputsLeft == 0 then 
          LDB.Del $ cachedTxKey $ txCacheRecHash info
         else
          LDB.Put (cachedTxKey $ txCacheRecHash info) (flat $ info { txCacheRecUnspentOutputsCount = outputsLeft })