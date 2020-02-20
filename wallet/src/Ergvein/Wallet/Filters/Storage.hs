module Ergvein.Wallet.Filters.Storage(
    FiltersT
  , FiltersStorage
  , HasFiltersStorage(..)
  , runFiltersStorage
  , openFiltersStorage
  , getFiltersHeight
  , insertFilter
  , getFilter
  ) where

import Control.Lens
import Control.Monad.Catch
import Control.Monad.Haskey
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text, unpack)
import Ergvein.Filters
import Ergvein.Types.Currency
import Ergvein.Wallet.Filters.Types
import Ergvein.Wallet.Native
import Network.Haskoin.Block

import qualified Ergvein.Wallet.Filters.Btc.Queries as BTC

type FiltersT m a = HaskeyT Schema m a
type FiltersStorage = ConcurrentDb Schema

-- | Name of filters sub storage folder in global storage folder
filtersStorageName :: Text
filtersStorageName = "filters"

getFiltersStoragePath :: (Monad m, HasStoreDir m) => m Text
getFiltersStoragePath = do
  st <- getStoreDir
  pure $ st <> "/" <> filtersStorageName

runFiltersStorage :: (MonadIO m, HasFiltersStorage m) => FiltersT IO a -> m a
runFiltersStorage ma = do
  db <- getFiltersStorage
  liftIO $ runHaskeyT ma db defFileStoreConfig

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

getFiltersHeight :: (MonadIO m, HasFiltersStorage m) => Currency -> m BlockHeight 
getFiltersHeight cur = case cur of 
  BTC -> runFiltersStorage $ transactReadOnly $ BTC.getFiltersHeight . (view schemaBtc)

insertFilter :: (MonadIO m, HasFiltersStorage m) => BlockHeight -> AddrFilter -> m ()
insertFilter h f = runFiltersStorage $ transact_ $ \schema -> case f of 
  AddrFilterBtc btcf -> do
    btcs <- BTC.insertFilter h btcf (view schemaBtc schema)
    commit_ $ schema & schemaBtc .~ btcs

getFilter :: (MonadIO m, HasFiltersStorage m) => Currency -> BlockHeight -> m (Maybe AddrFilter)
getFilter c bh = runFiltersStorage $ transactReadOnly $ \schema -> case c of 
  BTC -> fmap (fmap AddrFilterBtc) $ BTC.getFilter bh $ view schemaBtc schema