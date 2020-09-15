{-# LANGUAGE FlexibleInstances #-}
module Ergvein.Wallet.Native
  ( PlatformNatives(..)
  , HasStoreDir(..)
  , NativeAlerts(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone)
import Data.X509.CertificateStore (CertificateStore)
import Network.DNS.Resolver (ResolvConf)

class HasStoreDir m where
  getStoreDir :: m Text

instance Monad m => HasStoreDir (ReaderT Text m) where
  getStoreDir = ask
  {-# INLINE getStoreDir #-}

data NativeAlerts
  = NAFileDoesNotExist Text
  | NAFileIsEmpty Text
  | NADecodingError Text
  | NAGenericError Text
  deriving (Eq)

type AtomicMode = Bool

class PlatformNatives where
  -- | Make platform specific URL to given resource.
  resUrl :: Text -> Text

  -- | Key-value store. Write JSON value
  storeValue :: (HasStoreDir m, MonadIO m, ToJSON a) => Text -> a -> AtomicMode -> m ()

  -- | Key-value store. Read JSON value by key
  retrieveValue :: (HasStoreDir m, MonadIO m, FromJSON a) => Text -> a -> m (Either NativeAlerts a)

  -- | Return list of available keys to get with `retrieveValue`.
  listKeys :: (HasStoreDir m, MonadIO m) => m [Text]

  -- | Read stored file line by line from android app folder
  readStoredFile :: (HasStoreDir m, MonadIO m) => Text -> m (Either NativeAlerts [Text])

  -- | Write down a value to stored file at its end.
  appendStoredFile :: (HasStoreDir m, MonadIO m) => Text -> Text -> m ()

  -- | Move stored file from first name to the second with destruction of second.
  moveStoredFile :: (HasStoreDir m, MonadIO m) => Text -> Text -> m (Either NativeAlerts ())

  -- | Get size in bytes of stored file
  getStoreFileSize :: (HasStoreDir m, MonadIO m) => Text -> m (Either NativeAlerts Int)

  -- | Get contents of clipboard
  pasteStr :: MonadIO m => m Text

  -- | Put string into clipboard
  copyStr :: MonadIO m => Text -> m ()

  -- | Get the current time-zone.
  getTimeZone :: MonadIO m => m TimeZone

  -- | Share URL string for other applications
  shareUrl :: MonadIO m => Text -> m ()

  -- | Open URL in external browser
  openUrl :: MonadIO m => Text -> m ()

  -- | Work for camera of device
  cameraWork :: MonadIO m => Text -> m ()

  -- | Work for get result from camera of device
  cameraGetResult :: MonadIO m => m Text

  -- | Write to system log
  logWrite :: MonadIO m => Text -> m ()

  -- | Read system wide certificate store for TLS connections
  readSystemCertificates :: MonadIO m => m CertificateStore

  -- | Get system-appropriate DNS resolv config
  nativeResolvConf :: ResolvConf

  -- | Share a jpeg image. Implement for android only
  -- MonadIO m => jpegInBase64 -> Filename (w/o .jpeg) -> m ()
  nativeShareJpeg :: MonadIO m => Text -> Text -> m ()
