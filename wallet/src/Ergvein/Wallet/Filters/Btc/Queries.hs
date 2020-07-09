module Ergvein.Wallet.Filters.Btc.Queries(
    clearFiltersRange
  , insertFilter
  , insertMultipleFilters
  , readFilter
  , readFiltersHeight
  , foldFilters
  , scanFilters
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString
import Data.Foldable (traverse_)
import Data.Maybe
import Database.LMDB.Simple
import Database.LMDB.Simple.Extra
import Network.Haskoin.Block

import Ergvein.Filters.Btc.Mutable
import Ergvein.Text
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Wallet.Filters.Btc.Types
import Ergvein.Wallet.Platform

import qualified Database.LMDB.Simple.Extra as LMDB

clearFiltersRange :: BlockHeight -> BlockHeight -> Transaction ReadWrite ()
clearFiltersRange i0 i1 = do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  flip traverse_ [i0 .. i1] $ \i -> do
    mh <- get hdb i
    case mh of
      Nothing -> pure ()
      Just h -> do
        delete i hdb
        delete h fdb
        pure ()

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

readFilter :: MonadIO m => BlockHeight -> Environment ReadWrite -> m (Maybe BtcAddrFilter)
readFilter k e = liftIO . readOnlyTransaction e $ getFilter k

getFilter :: Mode mode => BlockHeight -> Transaction mode (Maybe BtcAddrFilter)
getFilter k = do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  fmap snd <$> getFilterImpl fdb hdb k

getFilterImpl :: Database BlockHash ByteString -> Database BlockHeight BlockHash ->  BlockHeight -> Transaction mode (Maybe (BlockHash, BtcAddrFilter))
getFilterImpl fdb hdb k = do
  mh <- get hdb k
  ffor31 maybe mh (pure Nothing) $ \h -> do
    mview <- get fdb h
    mfilter <- traverse decodeBtcAddrFilter mview
    pure $ maybe Nothing (either (const Nothing) (Just . (h,))) mfilter

readFiltersHeight :: MonadIO m => Environment ReadWrite -> m BlockHeight
readFiltersHeight e = liftIO $ readOnlyTransaction e getFiltersHeight

getFiltersHeight :: Mode mode => Transaction mode BlockHeight
getFiltersHeight = do
  tdb <- getBtcTotalDb
  fromMaybe (filterStartingHeight BTC) <$> get tdb ()

-- | Right fold over all filters
foldFilters :: forall a m . MonadIO m
  => (BlockHeight -> BlockHeight -> BlockHash -> BtcAddrFilter -> a -> IO a)
  -> a
  -> Environment ReadWrite
  -> m a
foldFilters f a0 e = liftIO . readOnlyTransaction e $ do
  fdb <- getBtcFiltersDb
  i1 <- getFiltersHeight
  io <- LMDB.foldrWithKey (f' i1) (pure (0, a0)) fdb
  fmap snd $ liftIO io
  where
    f' :: BlockHeight -> BlockHash -> ByteString -> IO (Int, a) -> IO (Int, a)
    f' i1 k bs calcAcc = do
      (!i0, !acc) <- calcAcc
      res <- decodeBtcAddrFilter bs
      case res of
        Left _ -> pure (i0, acc)
        Right a -> do
          acc' <- f (fromIntegral i0) i1 k a acc
          pure (i0+1, acc')
    {-# INLINABLE f' #-}

-- | Fold over filters that are not scanned yet
scanFilters :: forall a m . MonadIO m
  => BlockHeight -- ^ Starting height
  -> (BlockHeight -> BlockHeight -> BlockHash -> BtcAddrFilter -> a -> IO a)  -- ^ Scanner function
  -> a -- ^ initial value. Returned unchanged if there is no filters to scan
  -> Environment ReadWrite -- ^ Env with the DB
  -> m a -- ^ Result
scanFilters i0 f a0 e = liftIO . readOnlyTransaction e $ do
  fdb <- getBtcFiltersDb
  hdb <- getBtcHeightsDb
  i1 <- getFiltersHeight
  go fdb hdb i0 i1 a0
  where
    go fdb hdb !i0 i1 !acc
      | i0 > i1 = pure acc
      | otherwise = do
        mfilter <- getFilterImpl fdb hdb i0
        case mfilter of
          Nothing -> go fdb hdb (i0+1) i1 acc
          Just (h, mf) -> do
            !acc' <- liftIO $ f i0 i1 h mf acc
            go fdb hdb (i0+1) i1 acc'

ffor31 :: (a -> b -> c -> d) -> c -> a -> b -> d
ffor31 f c a b = f a b c
