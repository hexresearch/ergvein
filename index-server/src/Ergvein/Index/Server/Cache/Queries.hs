module Ergvein.Index.Server.Cache.Queries where

import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Schema
import Data.Flat
import Data.Default
import Database.LevelDB
import Database.LevelDB.Iterator
import qualified Database.LevelDB.Streaming as LDBStreaming
import Control.Lens
import Data.Maybe
import Data.ByteString as BS
import Ergvein.Types.Transaction

safeEntrySlice :: (MonadLDB m , Ord k, Flat k, Flat v) => BS.ByteString -> k -> m [(k,v)]
safeEntrySlice startKey endKey = do
  db <- getDb
  iterator <- createIter db def
  slice <- LDBStreaming.toList $ LDBStreaming.entrySlice iterator range LDBStreaming.Asc
  pure $ over _2 unflatExact  <$> over _1 unflatExact <$> slice
  where
    range = LDBStreaming.KeyRange startKey comparison
    comparison key = case unflat $ unPrefixedKey key of
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
  let result = fromJust notFoundErr maybeResult
      parsedResult = unflatExact result
  pure parsedResult
  where
    notFoundErr = error "getParsedExact error"


data TxOutHistoryItem = UTXO TxOutCacheRecItem | STXO (TxOutCacheRecItem, TxInCacheRec)

type TxOutHistory = [TxOutHistoryItem]

getTxOutHistory :: (MonadLDB m) => PubKeyScriptHash -> m (Maybe TxOutHistory)
getTxOutHistory key = do
  db <- getDb
  maybeHistory <- getParsed $ flat key
  case maybeHistory of
    Just history -> do
      txoHistory <- mapM withSpentInfo history
      pure $ Just txoHistory
    _ -> pure Nothing
  where
    withSpentInfo utxo = do
        maybeSTXO <- getParsed $ flat $ TxInCacheRecKey (txOutCacheRec'txHash utxo) (txOutCacheRec'index  utxo)
        pure $ case maybeSTXO of
          Just stxo -> STXO (utxo, stxo)
          _ -> UTXO utxo