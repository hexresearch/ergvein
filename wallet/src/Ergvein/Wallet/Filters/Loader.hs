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
import Ergvein.Index.API.Types
import Ergvein.Types.Currency
import Ergvein.Wallet.Client
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Alert

filtersLoader :: (HasFiltersStorage m, MonadFrontBase t m) => m ()
filtersLoader = nameSpace "filters loader" $ do
  sequence_ [filtersLoaderBtc]

filtersLoaderBtc :: (HasFiltersStorage m, MonadFrontBase t m) => m ()
filtersLoaderBtc = nameSpace "btc" $ do
  buildE <- getPostBuild
  he <- handleDangerMsg =<< getHeight (HeightRequest BTC <$ buildE)
  performEvent_ $ ffor he $ \h -> liftIO $ print h
  pure ()
