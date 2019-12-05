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
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Reflex.Dom

import qualified Data.ByteString.Lazy as BS

#ifdef ANDROID
import Ergvein.Wallet.Android.Native
#else
import Ergvein.Wallet.Desktop.Native
#endif

-- | Widget that writes down to internal storage all log entries
logWriter :: MonadFrontBase t m => Event t LogEntry -> m ()
logWriter e = performEvent_ $ ffor e $ \entry -> do
  size <- either (const 0) id <$> getStoreFileSize logStorageKey
  when (size >= logStorageMaxSize) $ void $ moveStoredFile logStorageKey logStorageKeyOld
  appendStoredFile logStorageKey $ (<> "\n") . decodeUtf8 . BS.toStrict . encode $ entry
