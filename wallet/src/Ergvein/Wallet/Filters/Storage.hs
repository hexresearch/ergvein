module Ergvein.Wallet.Filters.Storage(
    FiltersStorage
  , HasFiltersStorage(..)
  , openFiltersStorage
  , getFiltersHeight
  , insertFilter
  , insertMultipleFilters
  , getFilter
  , foldFilters
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Text (Text, unpack)
import Database.LMDB.Simple
import Network.Haskoin.Block
import System.Directory

import Ergvein.Filters.Mutable
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Wallet.Native

import qualified Ergvein.Wallet.Filters.Btc.Types as BTC
import qualified Ergvein.Wallet.Filters.Btc.Queries as BTC

type FiltersStorage = Environment ReadWrite

-- | Name of filters sub storage folder in global storage folder
filtersStorageName :: Text
filtersStorageName = "filters"

getFiltersStoragePath :: (Monad m, HasStoreDir m) => m Text
getFiltersStoragePath = do
  st <- getStoreDir
  pure $ st <> "/" <> filtersStorageName

openFiltersStorage :: (MonadIO m, MonadMask m, HasStoreDir m) => m FiltersStorage
openFiltersStorage = do
  fn <- getFiltersStoragePath
  let path = unpack fn
  liftIO $ do
    storeEx <- doesDirectoryExist path
    unless storeEx $ createDirectory path
  e <- liftIO $ openEnvironment path $ defaultLimits {
      maxDatabases = 6 -- TODO: update when we need more dbs for new currencies
    , mapSize = 1024 * 1024 * 4 * 1024 } -- 4 GB max size
  liftIO $ readWriteTransaction e BTC.initBtcDbs
  pure e

class Monad m => HasFiltersStorage m where
  getFiltersStorage :: m FiltersStorage

instance Monad m => HasFiltersStorage (ReaderT FiltersStorage m) where
  getFiltersStorage = ask
  {-# INLINE getFiltersStorage #-}

getFiltersHeight :: (MonadIO m, HasFiltersStorage m) => Currency -> m BlockHeight
getFiltersHeight cur = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.getFiltersHeight e

insertFilter :: (MonadIO m, HasFiltersStorage m) => Currency -> BlockHeight -> BlockHash -> AddressFilterHexView -> m ()
insertFilter cur h bh f = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.insertFilter h bh f e

insertMultipleFilters :: (MonadIO m, HasFiltersStorage m, Foldable t)
  => Currency
  -> t (BlockHeight, BlockHash, AddressFilterHexView)
  -> m ()
insertMultipleFilters cur fs = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.insertMultipleFilters fs e

getFilter :: (MonadIO m, HasFiltersStorage m) => Currency -> BlockHeight -> m (Maybe AddrFilter)
getFilter c bh = do
  e <- getFiltersStorage
  case c of
    BTC -> fmap (fmap AddrFilterBtc) $ BTC.getFilter bh e

-- | Right fold over filters.
foldFilters :: (MonadIO m, HasFiltersStorage m) => Currency -> (BlockHash -> AddrFilter -> a -> IO a) -> a -> m a
foldFilters c f a0 = do
  e <- getFiltersStorage
  case c of
    BTC -> BTC.foldFilters (\k -> f k . AddrFilterBtc) a0 e
    ERGO -> pure a0 -- ^ TODO: add ergo here
