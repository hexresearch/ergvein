{-# LANGUAGE BangPatterns #-}
module Sepulcas.Log.Reader(
    logReader
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.Maybe
import Data.Text.Encoding
import Reflex
import Sepulcas.Log.Monad
import Sepulcas.Log.Types
import Sepulcas.Native

import qualified Data.ByteString.Lazy as BS

-- | Getting logs and dynamically updates them
logReader :: (PerformEvent t m, MonadHold t m, MonadFix m, PostBuild t m, HasStoreDir (Performable m), MonadIO (Performable m), MonadNativeLogger t m, PlatformNatives)
  => m (Dynamic t [LogEntry])
logReader = do
  buildE <- getPostBuild
  initE <- performEvent $ ffor buildE $ const readLogEntries
  updE <- fst <$> getLogsTrigger
  foldDyn go [] $ leftmost [Right <$> updE, Left <$> initE]
  where
    go (Right a) !acc = a:acc
    go (Left !as) _ = as

-- | Extract log entries from internal storage
readLogEntries :: (HasStoreDir m, MonadIO m, PlatformNatives) => m [LogEntry]
readLogEntries = do
  newEs <- fmap (either (const []) id) $ readStoredFile logStorageKey
  oldEs <- fmap (either (const []) id) $ readStoredFile logStorageKeyOld
  let es = reverse newEs <> reverse oldEs
  pure $ catMaybes . fmap (decode . BS.fromStrict . encodeUtf8) $ es
