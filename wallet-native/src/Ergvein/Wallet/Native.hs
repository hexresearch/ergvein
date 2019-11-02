{-# LANGUAGE FlexibleInstances #-}
module Ergvein.Wallet.Native
  ( PlatformNatives(..)
  , HasStoreDir(..)
  ) where

import Control.Monad.Reader
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text (Text)

class HasStoreDir m where
  getStoreDir :: m Text

instance Monad m => HasStoreDir (ReaderT Text m) where
  getStoreDir = ask

class PlatformNatives where
  -- | Make platform specific URL to given resource.
  resUrl :: Text -> Text

  -- | Key-value store. Write JSON value
  storeValue :: (HasStoreDir m, MonadIO m, ToJSON a) => Text -> a -> m (Either Text ())

  -- | Key-value store. Read JSON value by key
  retrieveValue :: (HasStoreDir m, MonadIO m, FromJSON a) => Text -> a -> m (Either Text a)

  -- | Read stored file line by line from android app folder. Non existing file
  -- will return empty list.
  readStoredFile :: (HasStoreDir m, MonadIO m) => Text -> m [Text]

  -- | Write down a value to stored file at its end.
  appendStoredFile :: (HasStoreDir m, MonadIO m) => Text -> Text -> m ()

  -- | Move stored file from first name to the second with destruction of second.
  moveStoredFile :: (HasStoreDir m, MonadIO m) => Text -> Text -> m ()

  -- | Get size in bytes of stored file
  getStoreFileSize :: (HasStoreDir m, MonadIO m) => Text -> m Int

  -- | Get contents of clipboard
  pasteStr :: MonadIO m => m Text

  -- | Put string into clipboard
  copyStr :: MonadIO m => Text -> m ()
