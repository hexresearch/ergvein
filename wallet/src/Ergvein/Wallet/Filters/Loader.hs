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
  filtersLoader
) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Bifunctor
import Data.Maybe 
import Ergvein.Filters 
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Client
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Sync.Status
import Network.Haskoin.Block
import Reflex.Workflow

filtersLoader :: (HasFiltersStorage m, MonadFrontBase t m) => m ()
filtersLoader = nameSpace "filters loader" $ do
  sequence_ [filtersLoaderBtc]

filtersLoaderBtc :: MonadFrontBase t m => m ()
filtersLoaderBtc = nameSpace "btc" $ void $ workflow go 
  where 
    go = Workflow $ do
      buildE <- getPostBuild
      ch <- fmap fromIntegral $ getCurrentHeight BTC
      fh <- getFiltersHeight BTC
      logWrite $ "Current height is " <> showt ch <> ", and filters are for height " <> showt fh
      postSync BTC ch fh 
      if ch > fh then do 
        let n = 1000
        logWrite $ "Getting next " <> showt n <> " filters"
        fse <- getFilters ((BTC, fh+1, n) <$ buildE)
        we <- performFilters $ ffor fse $ \fs -> traverse_ (\(h, (bh, f)) -> insertFilter h bh f) (zip [fh+1 ..] fs) 
        pure ((), go <$ we)
      else do 
        logWrite "Sleeping, waiting for new filters ..."
        de <- delay 120 buildE
        pure ((), go <$ de)


postSync :: MonadFrontBase t m => Currency -> BlockHeight -> BlockHeight -> m ()
postSync cur ch fh = do 
  buildE <- getPostBuild
  setSyncProgress $ SyncMeta cur SyncFilters (fromIntegral fh) (fromIntegral ch) <$ buildE

getFilters :: MonadFrontBase t m => Event t (Currency, BlockHeight, Int) -> m (Event t [(BlockHash, AddrFilter)])
getFilters e = do 
  resE <- getBlockFilters $ ffor e $ \(cur, h, n) -> BlockFiltersRequest cur (fromIntegral h) (fromIntegral n)
  hexE <- handleDangerMsg resE 
  -- performEvent_ $ ffor hexE $ liftIO . print
  let decoder = decodeBtcAddrFilter <=< hex2bsTE
      mkFilter = either (const Nothing) (Just . AddrFilterBtc) . decoder 
      mkPair (a, b) = (,) <$> hexToBlockHash a <*> mkFilter b
  pure $ catMaybes . fmap mkPair <$> hexE