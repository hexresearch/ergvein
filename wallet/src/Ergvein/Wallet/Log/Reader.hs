{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Log.Reader(
    logReader
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Text(Text)
import Data.Text.Encoding
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Reflex.Dom

import qualified Data.ByteString.Lazy as BS

#ifdef ANDROID
import Ergvein.Wallet.Android.Native
#else
import Ergvein.Wallet.Desktop.Native
#endif

-- | Getting logs and dynamically updates them
logReader :: MonadFrontBase t m => m (Dynamic t [LogEntry])
logReader = do
  store <- getStoreDir
  buildE <- getPostBuild
  initE <- performEvent $ ffor buildE $ const (readLogEntries store)
  updE <- fst <$> getLogsTrigger
  foldDyn go [] $ leftmost [Right <$> updE, Left <$> initE]
  where
    go (Right a) !acc = a:acc
    go (Left !as) _ = as

-- | Extract log entries from internal storage
readLogEntries :: MonadIO m => Text -> m [LogEntry]
readLogEntries store = flip runReaderT store $ do
  newEs <- fmap (either (const []) id) $ readStoredFile logStorageKey
  oldEs <- fmap (either (const []) id) $ readStoredFile logStorageKeyOld
  let es = reverse newEs <> reverse oldEs
  pure $ catMaybes . fmap (decode . BS.fromStrict . encodeUtf8) $ es
