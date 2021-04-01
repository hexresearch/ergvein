{-# LANGUAGE CPP #-}
module Sepulcas.Log.Writer(
    logWriter
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text.Encoding
import Reflex
import Sepulcas.Log.Types
import Sepulcas.Native

import qualified Data.ByteString.Lazy as BS

-- | Widget that writes down to internal storage all log entries
logWriter :: (PerformEvent t m, PlatformNatives, HasStoreDir (Performable m), MonadIO (Performable m)) => Event t LogEntry -> m ()
logWriter e = performEvent_ $ ffor e $ \entry -> do
  size <- either (const 0) id <$> getStoreFileSize logStorageKey
  when (size >= logStorageMaxSize) $ void $ moveStoredFile logStorageKey logStorageKeyOld
  appendStoredFile logStorageKey $ (<> "\n") . decodeUtf8 . BS.toStrict . encode $ entry
