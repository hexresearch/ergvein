module Ergvein.Index.Server.Cache.Queries where

import Ergvein.Index.Server.Cache.Monad
import Ergvein.Index.Server.Cache.Schema
import Data.Flat
import Data.Default
import Database.LevelDB
import Database.LevelDB.Iterator
import qualified Database.LevelDB.Streaming as LDBStreaming
import Control.Lens

safeEntrySlice :: (MonadLDB m , Ord k, Flat k, Flat v) => k -> k -> m [(k,v)]
safeEntrySlice startKey endKey = do
  db <- getDb
  iterator <- createIter db def
  slice <- LDBStreaming.toList $ LDBStreaming.entrySlice iterator range LDBStreaming.Asc
  pure $ over _2 unflatExact  <$> over _1 unflatExact <$> slice
  where
    start = flat startKey
    range = LDBStreaming.KeyRange start comparison
    comparison key = case unflat $ unPrefixedKey key of
      Right parsedKey -> compare parsedKey endKey
      _ -> GT