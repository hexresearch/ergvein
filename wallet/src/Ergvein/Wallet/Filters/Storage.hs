module Ergvein.Wallet.Filters.Storage(
    FiltersT
  , FiltersStorage
  , HasFiltersStorage(..)
  , runFiltersStorage
  , openFiltersStorage
  , performFilters
  , getFiltersHeight
  , insertFilter
  , getFilter
  , foldFilters
  ) where

import Control.Concurrent
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
import Reflex.Dom 

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

-- | Helper to perform actions in store concurrently in reflex network.
performFilters :: (PerformEvent t m, TriggerEvent t m, HasFiltersStorage (Performable m), MonadIO (Performable m)) 
  => Event t (ReaderT FiltersStorage IO a) -> m (Event t a)
performFilters e = performEventAsync $ ffor e $ \ma fire -> do 
  s <- getFiltersStorage
  void . liftIO . forkIO $ fire =<< runReaderT ma s

getFiltersHeight :: (MonadIO m, HasFiltersStorage m) => Currency -> m BlockHeight 
getFiltersHeight cur = case cur of 
  BTC -> runFiltersStorage $ transactReadOnly $ BTC.getFiltersHeight . (view schemaBtc)

insertFilter :: (MonadIO m, HasFiltersStorage m) => BlockHeight -> BlockHash -> AddrFilter -> m ()
insertFilter h bh f = runFiltersStorage $ transact_ $ \schema -> case f of 
  AddrFilterBtc btcf -> do
    btcs <- BTC.insertFilter h bh btcf (view schemaBtc schema)
    commit_ $ schema & schemaBtc .~ btcs

getFilter :: (MonadIO m, HasFiltersStorage m) => Currency -> BlockHeight -> m (Maybe AddrFilter)
getFilter c bh = runFiltersStorage $ transactReadOnly $ \schema -> case c of 
  BTC -> fmap (fmap AddrFilterBtc) $ BTC.getFilter bh $ view schemaBtc schema

-- | Right fold over filters.
foldFilters :: (MonadIO m, HasFiltersStorage m) => Currency -> (BlockHash -> AddrFilter -> a -> a) -> a -> m a 
foldFilters c f a0 = runFiltersStorage $ transactReadOnly $ case c of 
  BTC -> BTC.foldFilters (\k -> f k . AddrFilterBtc) a0 . view schemaBtc