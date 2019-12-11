module Ergvein.Wallet.Headers.Storage(
    HeadersT
  , HeadersStorage
  , HasHeadersStorage(..)
  , runHeadersStorage
  , openHeadersStorage
  ) where

import Control.Monad.Catch
import Control.Monad.Haskey
import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import Ergvein.Wallet.Headers.Types
import Ergvein.Wallet.Native

type HeadersT m a = HaskeyT Schema m a
type HeadersStorage = ConcurrentDb Schema

-- | Name of headers sub storage folder in global storage folder
headersStorageName :: Text
headersStorageName = "headers"

getHeadersStoragePath :: (Monad m, HasStoreDir m) => m Text
getHeadersStoragePath = do
  st <- getStoreDir
  pure $ st <> "/" <> headersStorageName

runHeadersStorage :: (MonadIO m, MonadMask m) => HeadersT m a -> HeadersStorage -> m a
runHeadersStorage ma db = runHaskeyT ma db defFileStoreConfig

openHeadersStorage :: (MonadIO m, MonadMask m, HasStoreDir m) => m HeadersStorage
openHeadersStorage = do
  fn <- getHeadersStoragePath
  let hnds = concurrentHandles . unpack $ fn
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
        Nothing -> createConcurrentDb hnds emptySchema
        Just db -> return db

class HasHeadersStorage m where
  getHeadersStorage :: m HeadersStorage
