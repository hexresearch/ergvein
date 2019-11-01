{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Log.Writer(
    logWriter
  ) where

import Control.Monad
import Control.Monad.Reader
import Data.Text(Text)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text.Encoding
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Reflex.Dom

import qualified Data.ByteString.Lazy as BS

#ifdef ANDROID
import Ergvein.Wallet.Android.Native
#else
import Ergvein.Wallet.Desktop.Native
#endif

instance MonadIO m => HasStoreDir (ReaderT Text m) where
  getStoreDir = ask

-- | Widget that writes down to internal storage all log entries
logWriter :: MonadFrontBase t m => Event t LogEntry -> m ()
logWriter e = do
  store <- getStoreDir
  performEvent_ $ (writeEntry store) <$> e

writeEntry :: MonadIO m => Text -> LogEntry -> m ()
writeEntry store e = flip runReaderT store $ do
  size <- getStoreFileSize logStorageKey
  when (size >= logStorageMaxSize) $ moveStoredFile logStorageKey logStorageKeyOld
  appendStoredFile logStorageKey $ (<> "\n") . decodeUtf8 . BS.toStrict . encode $ e
