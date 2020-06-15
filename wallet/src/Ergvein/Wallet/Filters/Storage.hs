module Ergvein.Wallet.Filters.Storage(
    FiltersStorage
  , HasFiltersStorage(..)
  , openFiltersStorage
  , getFiltersHeight
  , getScannedHeight
  , watchFiltersHeight
  , writeFiltersHeight
  , watchScannedHeight
  , writeScannedHeight
  , insertFilter
  , insertMultipleFilters
  , getFilter
  , foldFilters
  , scanFilters
  , scanBtcFilters
  ) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text, unpack)
import Database.LMDB.Simple
import Network.Haskoin.Block
import Reflex
import Reflex.ExternalRef
import System.Directory

import Ergvein.Filters.Mutable
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform

import qualified Data.Map.Strict as M
import qualified Ergvein.Wallet.Filters.Btc.Queries as BTC
import qualified Ergvein.Wallet.Filters.Btc.Types as BTC

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

class Monad m => HasFiltersStorage t m | m -> t where
  getFiltersStorage :: m FiltersStorage
  getFiltersHeightRef :: m (ExternalRef t (Map Currency BlockHeight))
  getScannedHeightRef :: m (ExternalRef t (Map Currency BlockHeight))

getFiltersHeight :: (MonadIO m, HasFiltersStorage t m) => Currency -> m BlockHeight
getFiltersHeight cur = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.readFiltersHeight e
    ERGO -> pure $ filterStartingHeight ERGO -- TODO: here

watchFiltersHeight :: (MonadIO m, HasFiltersStorage t m, MonadHold t m, Reflex t, MonadFix m) => Currency -> m (Dynamic t BlockHeight)
watchFiltersHeight cur = do
  md <- externalRefDynamic =<< getFiltersHeightRef
  holdUniqDyn $ (fromMaybe (filterStartingHeight cur) . M.lookup cur) <$> md

writeFiltersHeight :: (MonadIO m, HasFiltersStorage t m) => Currency -> BlockHeight -> m ()
writeFiltersHeight cur h = do
  r <- getFiltersHeightRef
  modifyExternalRef_ r $ M.insert cur h

getScannedHeight :: (MonadIO m, HasFiltersStorage t m, MonadHold t m, Reflex t, MonadFix m) => Currency -> m BlockHeight
getScannedHeight cur = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.readScannedHeight e
    ERGO -> pure $ filterStartingHeight ERGO -- TODO: here

watchScannedHeight :: (MonadIO m, HasFiltersStorage t m, MonadHold t m, Reflex t, MonadFix m) => Currency -> m (Dynamic t BlockHeight)
watchScannedHeight cur = do
  md <- externalRefDynamic =<< getScannedHeightRef
  holdUniqDyn $ (fromMaybe (filterStartingHeight cur) . M.lookup cur) <$> md

writeScannedHeight :: (MonadIO m, HasFiltersStorage t m) => Currency -> BlockHeight -> m ()
writeScannedHeight cur h = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.writeScannedHeight e h
    ERGO -> pure () -- TODO: here
  r <- getScannedHeightRef
  modifyExternalRef_ r $ M.insert cur h

insertFilter :: (MonadIO m, HasFiltersStorage t m) => Currency -> BlockHeight -> BlockHash -> AddressFilterHexView -> m ()
insertFilter cur h bh f = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.insertFilter h bh f e
    ERGO -> pure () -- TODO: here
  writeFiltersHeight cur =<< getFiltersHeight cur

insertMultipleFilters :: (MonadIO m, HasFiltersStorage t m, Foldable f)
  => Currency
  -> f (BlockHeight, BlockHash, AddressFilterHexView)
  -> m ()
insertMultipleFilters cur fs = do
  e <- getFiltersStorage
  case cur of
    BTC -> BTC.insertMultipleFilters fs e
    ERGO -> pure () -- TODO: here
  writeFiltersHeight cur =<< getFiltersHeight cur

getFilter :: (MonadIO m, HasFiltersStorage t m) => Currency -> BlockHeight -> m (Maybe AddrFilter)
getFilter c bh = do
  e <- getFiltersStorage
  case c of
    BTC -> fmap (fmap AddrFilterBtc) $ BTC.readFilter bh e
    ERGO -> pure Nothing -- TODO: here

-- | Right fold over filters.
foldFilters :: (MonadIO m, HasFiltersStorage t m) => Currency -> (BlockHash -> AddrFilter -> a -> IO a) -> a -> m a
foldFilters c f a0 = do
  e <- getFiltersStorage
  case c of
    BTC -> BTC.foldFilters (\k -> f k . AddrFilterBtc) a0 e
    ERGO -> pure a0 -- ^ TODO: add ergo here

-- | Fold over filters that are not scanned yet.
scanFilters :: (MonadIO m, HasFiltersStorage t m) => Currency -> (BlockHeight -> BlockHeight -> BlockHash -> AddrFilter -> a -> IO a) -> a -> m a
scanFilters c f a0 = case c of
  BTC -> scanBtcFilters (\i n k -> f i n k . AddrFilterBtc) a0
  ERGO -> pure a0 -- ^ TODO: add ergo here

-- | Fold over filters that are not scanned yet.
scanBtcFilters :: (MonadIO m, HasFiltersStorage t m) => (BlockHeight -> BlockHeight -> BlockHash -> BtcAddrFilter -> a -> IO a) -> a -> m a
scanBtcFilters f a0 = do
  e <- getFiltersStorage
  BTC.scanFilters (\i n k -> f i n k) a0 e
