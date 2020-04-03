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

import Data.ByteString as BS
import qualified Data.Serialize as S
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming

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


data TxOutHistoryItem = UTXO TxOutCacheRecItem | STXO (TxOutCacheRecItem, TxInCacheRec)

type TxOutHistory = [TxOutHistoryItem]

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

getTxOutHistory :: (MonadLDB m) => PubKeyScriptHash -> m (Maybe TxOutHistory)
getTxOutHistory key = do
  db <- getDb
  maybeHistory <- getParsed $ cachedTxOutKey key
  case maybeHistory of
    Just history -> do
      txoHistory <- mapM withSpentInfo history
      pure $ Just txoHistory
    _ -> pure Nothing
  where
    withSpentInfo utxo = do
        maybeSTXO <- getParsed $ cachedTxInKey (txOutCacheRecTxHash utxo, txOutCacheRecIndex  utxo)
        pure $ case maybeSTXO of
          Just stxo -> STXO (utxo, stxo)
          _ -> UTXO utxo