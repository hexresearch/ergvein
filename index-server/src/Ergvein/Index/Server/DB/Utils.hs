{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Utils
  (
    unflatExact
  , decodeExact
  , unPrefixedKey
  , parsedKey
  , safeEntrySlice
  , getParsed
  , getParsedExact
  , getManyParsedExact
  , putItems
  , putItem
  , upsertItem
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Logger
import Conversion
import Crypto.Hash.SHA256
import Data.ByteString (ByteString)
import Data.Default
import Data.Either
import Data.FileEmbed
import Data.Flat
import Data.Foldable
import Data.Maybe
import Data.Serialize (Serialize)
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.Time.Clock
import Data.Word
import Database.LevelDB
import Database.LevelDB.Iterator
import Ergvein.Index.Server.Dependencies
import Servant.Client.Core
import System.ByteOrder

-- import Ergvein.Index.Server.DB.Monad
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString as BS
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Serialize as S
import qualified Data.Text as T
import qualified Database.LevelDB as LDB
import qualified Database.LevelDB.Streaming as LDBStreaming

instance Serialize Text where
  put txt = S.put $ encodeUtf8 txt
  get     = decodeUtf8 <$> S.get

unflatExact :: (Flat a) => ByteString -> a
unflatExact s = case unflat s of
    Right k -> k
    Left e -> error $ show e ++ "value " ++  show s

decodeExact :: (Serialize b) => ByteString -> b
decodeExact s = case S.decode s of
    Right k -> k
    Left e -> error e

unPrefixedKey :: ByteString -> ByteString
unPrefixedKey key = BS.tail key
  where err = error $ "unPrefixedKey error"

parsedKey :: Serialize k => ByteString -> k
parsedKey = fromRight (error "ser") . S.decode . unPrefixedKey

safeEntrySlice :: (MonadIO m , Ord k, S.Serialize k, Flat v) => DB -> BS.ByteString -> k -> m [(k,v)]
safeEntrySlice db startKey endKey = do
  iterator <- createIter db def
  slice <- LDBStreaming.toList $ LDBStreaming.entrySlice iterator range LDBStreaming.Asc
  pure $ over _2 unflatExact  <$> over _1 (decodeExact . unPrefixedKey) <$> slice
  where
    range = LDBStreaming.KeyRange startKey comparison
    comparison key = case S.decode $ unPrefixedKey key of
      Right parsedKey -> compare parsedKey endKey
      _ -> GT

getParsed :: (Flat v, MonadIO m) => DB -> BS.ByteString -> m (Maybe v)
getParsed db key = do
  maybeResult <- get db def key
  let maybeParsedResult = unflatExact <$> maybeResult
  pure maybeParsedResult

getParsedExact :: (Flat v, MonadIO m, MonadLogger m) => Text -> DB -> BS.ByteString -> m v
getParsedExact caller db key = do
  maybeResult <- get db def key
  case maybeResult of
    Just result -> pure $ unflatExact result
    Nothing -> do
      currentTime <- liftIO getCurrentTime
      logErrorN $ "[Db read miss][getParsedExact]" <> "["<> caller <>"]"<> " Entity with key " <> (T.pack $ show key) <> " not found at time:" <> (T.pack $ show currentTime)
      error $ "getParsedExact: not found" ++ show key

getManyParsedExact :: (Flat v, MonadIO m, MonadLogger m) => Text -> DB -> [BS.ByteString] -> m [v]
getManyParsedExact caller db keys = traverse (getParsedExact caller db) keys

putItems :: (Flat v) => (a -> BS.ByteString) -> (a -> v) -> [a] -> LDB.WriteBatch
putItems keySelector valueSelector items = putI <$> items
  where putI item = LDB.Put (keySelector item) $ flat $ valueSelector item

putItem :: (Flat v) => BS.ByteString -> v -> LDB.WriteBatch
putItem key item = [LDB.Put key $ flat item]

upsertItem :: (Flat v, MonadIO m, MonadLogger m) => DB -> BS.ByteString -> v -> m ()
upsertItem db key item = do
  put db def key $ flat item
