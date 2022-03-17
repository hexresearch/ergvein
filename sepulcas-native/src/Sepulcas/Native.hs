{-# LANGUAGE FlexibleInstances #-}
module Sepulcas.Native
  ( PlatformNatives(..)
  , HasStoreDir(..)
  , NativeAlerts(..)
  , Platform(..)
  , isDesktop
  , isAndroid
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.LocalTime (TimeZone)
import Data.X509.CertificateStore (CertificateStore)
import GHC.Generics (Generic)

class HasStoreDir m where
  getStoreDir :: m Text

instance {-# OVERLAPPABLE #-} Monad m => HasStoreDir (ReaderT Text m) where
  getStoreDir = ask
  {-# INLINE getStoreDir #-}

data NativeAlerts
  = NAFileDoesNotExist Text
  | NAFileIsEmpty Text
  | NADecodingError Text
  | NAGenericError Text
  deriving (Eq, Show)

type AtomicMode = Bool

-- | Platform the wallet is compiled for.
data Platform = DesktopLinux | Android
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- | Helpers to test current platform
isDesktop, isAndroid :: PlatformNatives => Bool
isDesktop = currentPlatform == DesktopLinux
isAndroid = currentPlatform == Android

class PlatformNatives where
  -- | Get current platform of wallet
  currentPlatform :: Platform

  -- | Get where application can store it files
  getHomeDir :: MonadIO m => m Text

  -- | Make platform specific URL to given resource.
  resUrl :: Text -> Text

  -- | ByteString storage. Write ByteString
  storeBS :: (HasStoreDir m, MonadIO m) => Text -> ByteString -> AtomicMode -> m ()

  -- | ByteString storage. Read ByteString
  retrieveBS :: (HasStoreDir m, MonadIO m) => Text -> m (Either NativeAlerts ByteString)

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

  -- | Delete stored file
  deleteStoredFile :: (HasStoreDir m, MonadIO m) => Text -> m ()

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

  -- | Share a jpeg image. Implement for android only
  -- MonadIO m => jpegInBase64 -> Filename (w/o .jpeg) -> m ()
  nativeShareJpeg :: MonadIO m => Text -> Text -> m ()

  -- | Detect dns servers. Used for Android
  androidDetectDns :: MonadIO m => m [String]


  androidSetScreenFlag :: MonadIO m => m ()
  androidClearScreenFlag :: MonadIO m => m ()
