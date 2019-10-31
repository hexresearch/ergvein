module Ergvein.Wallet.Native
  ( PlatformNatives(..)
  ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)

class PlatformNatives where
  -- | Make platform specific URL to given resource.
  resUrl :: Text -> Text

  -- | Key-value store. Write JSON value
  storeValue :: (MonadIO m, ToJSON a) => Text -> a -> m (Either Text ())

  -- | Key-value store. Read JSON value by key
  retrieveValue :: (MonadIO m, FromJSON a) => Text -> a -> m (Either Text a)

  -- | Read stored file line by line from android app folder. Non existing file
  -- will return empty list.
  readStoredFile :: (MonadIO m) => Text -> m [Text]

  -- | Write down a value to stored file at its end.
  appendStoredFile :: (MonadIO m) => Text -> Text -> m ()

  -- | Move stored file from first name to the second with destruction of second.
  moveStoredFile :: (MonadIO m) => Text -> Text -> m ()

  -- | Get size in bytes of stored file
  getStoreFileSize :: MonadIO m => Text -> m Int

  -- | Get contents of clipboard
  pasteStr :: MonadIO m => m Text

  -- | Put string into clipboard
  copyStr :: MonadIO m => Text -> m ()

  -- | Get storage path
  getStoreDir :: MonadIO m => m (Maybe FilePath)
