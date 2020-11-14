-----------------------------------------------------------------------------
--
-- Module      :  Ergvein.Wallet.Filters.Loader
-- Copyright   :  2019 ATUM SOLUTIONS AG
-- License     :  MIT
--
-- Maintainer  :  Anton Gushcha <ncrashed@protonmail.com>, Vladimir Krutkin <krutkinvs@gmail.com>
-- Stability   :
-- Portability :
--
-- | Contains thread that continuously loads golomb rice filters from indexers.
--
-----------------------------------------------------------------------------

module Ergvein.Wallet.Filters.Loader (
  getFilters
) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Maybe
-- import Network.Haskoin.Block

import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

import qualified Data.Vector as V

filtersLoader :: MonadFront t m => m ()
filtersLoader = nameSpace "filters loader" $ do
  sequence_ [filtersLoaderBtc]

filtersLoaderBtc :: MonadFront t m => m ()
filtersLoaderBtc = nameSpace "btc" $ void $ workflow go
  where
    go = Workflow $ do
      buildE <- getPostBuild
      ch  <- fmap fromIntegral . sample . current =<< getCurrentHeight BTC
      sh  <- fmap fromIntegral . sample . current =<< getWalletsScannedHeightD BTC
      fh' <- getFiltersHeight BTC
      let fh = max fh' sh
      logWrite $ "Current height is " <> showt ch <> ", and filters are for height " <> showt fh
      if ch > fh then do
        let n = 500
        logWrite $ "Getting next filters ..."
        fse <- getFilters BTC $ (fh+1, n) <$ buildE
        performEvent_ $ ffor fse $ \fs -> logWrite $ "Got " <> showt (length fs) <> " filters!"
        we <- performFork $ ffor fse $ \fs ->
          insertMultipleFilters BTC $ zipWith (\h (bh,f) -> (h,bh,f)) [fh+1..] fs
        goE <- fmap (go <$) $ delay 1 we
        pure ((), goE)
      else do
        logWrite "Sleeping, waiting for new filters ..."
        let dt = if ch == 0 then 10 else 120
        de <- delay dt buildE
        upde <- updated <$> getCurrentHeight BTC
        pure ((), go <$ leftmost [de, void upde])

getFilters :: MonadFront t m => Currency -> Event t (BlockHeight, Int) -> m (Event t [(BlockHash, ByteString)])
getFilters cur e = do
  respE <- requestRandomIndexer $ ffor e $ \(h, n) -> (cur, ) $
    MFiltersRequest $ FilterRequest curcode (fromIntegral h) (fromIntegral n)
  let respE' = fforMaybe respE $ \case
        (addr, MFiltersResponse (FilterResponse{..})) -> if filterResponseCurrency /= curcode
          then Nothing
          else Just $ (addr,) $ V.toList $ ffor filterResponseFilters $ \(BlockFilter bid filt) -> (bid, filt)
        _ -> Nothing
  warnDesynced respE'
  pure $ snd <$> respE'
  where
    curcode = currencyToCurrencyCode cur

warnDesynced :: MonadFront t m => Event t (SockAddr, [a]) -> m ()
warnDesynced e = showWarnMsg $ fforMaybe e $ \(addr, rs) -> if null rs
  then Just $ "Indexer " <> showt addr <> " possibly out of sync!"
  else Nothing
