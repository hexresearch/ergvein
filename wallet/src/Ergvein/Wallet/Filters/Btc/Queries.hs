module Ergvein.Wallet.Filters.Btc.Queries(
    insertFilter
  , getFilter
  , getFiltersHeight
  , foldFilters
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Database.LMDB.Simple
import Ergvein.Filters.Btc
import Ergvein.Wallet.Filters.Btc.Types
import Ergvein.Wallet.Platform
import Network.Haskoin.Block

import qualified Database.LMDB.Simple.Extra as LMDB

insertFilter :: MonadIO m => BlockHeight -> BlockHash -> BtcAddrFilter -> Environment ReadWrite -> m ()
insertFilter h bh f e = liftIO . readWriteTransaction e $ do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  put fdb bh $ Just f
  put hdb h $ Just bh
  mtotal <- get tdb ()
  case mtotal of
    Just total | total >= h -> pure ()
    _ -> put tdb () $ Just h

getFilter :: MonadIO m => BlockHeight -> Environment ReadWrite -> m (Maybe BtcAddrFilter)
getFilter k e = liftIO . readOnlyTransaction e $ do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  mh <- get hdb k
  case mh of
    Nothing -> pure Nothing
    Just h -> get fdb h

-- | We start to download filters for BTC when SegWit was activated and recently on the testnet.
startHeight :: BlockHeight
startHeight = if isTestnet then 1722763 else 481824

getFiltersHeight :: MonadIO m => Environment ReadWrite -> m BlockHeight
getFiltersHeight e = liftIO . readOnlyTransaction e $ do
  tdb <- getBtcTotalDb
  fromMaybe startHeight <$> get tdb ()

-- | Right fold over all filters
foldFilters :: MonadIO m =>  (BlockHash -> BtcAddrFilter -> a -> a) -> a -> Environment ReadWrite -> m a
foldFilters f a0 e = liftIO . readOnlyTransaction e $ do
  fdb <- getBtcFiltersDb
  LMDB.foldrWithKey f a0 fdb
