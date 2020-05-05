module Ergvein.Wallet.Filters.Btc.Queries(
    insertFilter
  , insertMultipleFilters
  , getFilter
  , getFiltersHeight
  , foldFilters
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import Data.Foldable (traverse_)
import Data.Maybe
import Database.LMDB.Simple
import Network.Haskoin.Block

import Ergvein.Filters.Btc
import Ergvein.Text
import Ergvein.Types.Block
import Ergvein.Wallet.Filters.Btc.Types

import qualified Database.LMDB.Simple.Extra as LMDB

insertFilter :: MonadIO m => BlockHeight -> BlockHash -> AddressFilterHexView -> Environment ReadWrite -> m ()
insertFilter h bh fview e = liftIO . readWriteTransaction e $ do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  ffor31 either (hex2bsTE fview) (const $ pure ()) $ \f -> do
    put fdb bh $ Just f
    put hdb h $ Just bh
    mtotal <- get tdb ()
    case mtotal of
      Just total | total >= h -> pure ()
      _ -> put tdb () $ Just h

insertMultipleFilters :: (MonadIO m, Foldable t) => t (BlockHeight, BlockHash, AddressFilterHexView) -> Environment ReadWrite -> m ()
insertMultipleFilters fs e = liftIO . readWriteTransaction e $ do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  tdb <- getBtcTotalDb
  flip traverse_ fs $ \(h,bh,fview) -> ffor31 either (hex2bsTE fview) (const $ pure ()) $ \f -> do
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
  ffor31 maybe mh (pure Nothing) $ \h -> do
    mview <- get fdb h
    pure $ maybe Nothing (either (const Nothing) Just . decodeBtcAddrFilter) mview

getFiltersHeight :: MonadIO m => Environment ReadWrite -> m BlockHeight
getFiltersHeight e = liftIO . readOnlyTransaction e $ do
  tdb <- getBtcTotalDb
  fromMaybe 0 <$> get tdb ()

-- | Right fold over all filters
foldFilters :: forall a m . MonadIO m
  => (BlockHash -> BtcAddrFilter -> a -> a)
  -> a
  -> Environment ReadWrite
  -> m a
foldFilters f a0 e = liftIO . readOnlyTransaction e $ do
  fdb <- getBtcFiltersDb
  LMDB.foldrWithKey f' a0 fdb
  where
    f' :: BlockHash -> ByteString -> a -> a
    f' k bs acc = case decodeBtcAddrFilter bs of
      Left _ -> acc
      Right a -> f k a acc

ffor31 :: (a -> b -> c -> d) -> c -> a -> b -> d
ffor31 f c a b = f a b c
