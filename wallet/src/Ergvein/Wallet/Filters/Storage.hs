module Ergvein.Wallet.Filters.Storage(
    FiltersT
  , FiltersStorage
  , HasFiltersStorage(..)
  , runFiltersStorage
  , openFiltersStorage
  ) where

import Control.Monad.Catch
import Control.Monad.Haskey
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text, unpack)
import Ergvein.Wallet.Filters.Types
import Ergvein.Wallet.Native

type FiltersT m a = HaskeyT Schema m a
type FiltersStorage = ConcurrentDb Schema

-- | Name of filters sub storage folder in global storage folder
filtersStorageName :: Text
filtersStorageName = "filters"

getFiltersStoragePath :: (Monad m, HasStoreDir m) => m Text
getFiltersStoragePath = do
  st <- getStoreDir
  pure $ st <> "/" <> filtersStorageName

runFiltersStorage :: (MonadIO m, MonadMask m) => FiltersT m a -> FiltersStorage -> m a
runFiltersStorage ma db = runHaskeyT ma db defFileStoreConfig

openFiltersStorage :: (MonadIO m, MonadMask m, HasStoreDir m) => m FiltersStorage
openFiltersStorage = do
  fn <- getFiltersStoragePath
  let hnds = concurrentHandles . unpack $ fn
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
        Nothing -> createConcurrentDb hnds emptySchema
        Just db -> return db

class Monad m => HasFiltersStorage m where
  getFiltersStorage :: m FiltersStorage

instance Monad m => HasFiltersStorage (ReaderT FiltersStorage m) where
  getFiltersStorage = ask
  {-# INLINE getFiltersStorage #-}
