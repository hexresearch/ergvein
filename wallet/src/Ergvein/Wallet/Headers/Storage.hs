module Ergvein.Wallet.Headers.Storage(
    HeadersStorage
  , HasHeadersStorage(..)
  , openHeadersStorage
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text, unpack)
import Database.LMDB.Simple
import Ergvein.Wallet.Headers.Types
import Ergvein.Wallet.Native
import System.Directory

import qualified Ergvein.Wallet.Headers.Btc.Types as BTC

-- | Name of filters sub storage folder in global storage folder
headersStorageName :: Text
headersStorageName = "filters"

getHeadersStoragePath :: (Monad m, HasStoreDir m) => m Text
getHeadersStoragePath = do
  st <- getStoreDir
  pure $ st <> "/" <> headersStorageName

openHeadersStorage :: (MonadIO m, MonadMask m, HasStoreDir m) => m HeadersStorage
openHeadersStorage = do
  fn <- getHeadersStoragePath
  let path = unpack fn
  liftIO $ do
    storeEx <- doesDirectoryExist path
    unless storeEx $ createDirectory path
  e <- liftIO $ openEnvironment path $ defaultLimits {
      maxDatabases = 6 -- TODO: update when we need more dbs for new currencies
    , mapSize = 1024 * 1024 * 4 * 1024 } -- 4 GB max size
  liftIO $ readWriteTransaction e BTC.initBtcDbs
  pure e
