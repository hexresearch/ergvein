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
import Data.Maybe
import Network.Haskoin.Block
import Reflex.ExternalRef

import Ergvein.Filters
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Block
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Client
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Sync.Status

import qualified Data.Map as M

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
      -- postSync BTC ch fh
      if ch > fh then do
        let n = 500
        logWrite $ "Getting next filters ..." <> showt n
        fse <- getFilters ((BTC, fh+1, n) <$ buildE)
        performEvent_ $ ffor fse $ const $ logWrite "Got filters!"
        we <- performFork $ ffor fse $ \fs ->
          insertMultipleFilters BTC $ zipWith (\h (bh,f) -> (h,bh,f)) [fh+1..] fs
        goE <- fmap (go <$) $ delay 1 we
        pure ((), goE)
      else do
        logWrite "Sleeping, waiting for new filters ..."
        let dt = if ch == 0 then 1 else 120
        de <- delay dt buildE
        upde <- updated <$> getCurrentHeight BTC
        pure ((), go <$ leftmost [de, void upde])

postSync :: MonadFront t m => Currency -> BlockHeight -> BlockHeight -> m ()
postSync cur ch fh = do
  syncD <- getSyncProgress
  sp <- sample . current $ syncD
  let shouldUpdate = case sp of
        Synced -> True
        SyncMeta{..} -> syncMetaStage == SyncFilters
  when shouldUpdate $ do
    buildE <- getPostBuild
    let val = if fh >= ch
          then Synced
          else SyncMeta cur SyncFilters (fromIntegral fh) (fromIntegral ch)
    setFiltersSync cur $ val == Synced
    setSyncProgress $ val <$ buildE

getFilters :: MonadFront t m => Event t (Currency, BlockHeight, Int) -> m (Event t [(BlockHash, AddressFilterHexView)])
getFilters e = do
  resE <- getBlockFiltersRandom $ ffor e $ \(cur, h, n) ->
    BlockFiltersRequest cur (fromIntegral h) (fromIntegral n)
  hexE <- handleDangerMsg resE
  let mkPair (a, b) = (, b) <$> hexToBlockHash a
  pure $ fmap (catMaybes .  fmap mkPair) hexE
