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

filtersLoader :: (HasFiltersStorage m, MonadFrontBase t m) => m ()
filtersLoader = nameSpace "filters loader" $ do
  sequence_ [filtersLoaderBtc]

filtersLoaderBtc :: (HasFiltersStorage m, MonadFrontBase t m) => m ()
filtersLoaderBtc = nameSpace "btc" $ do
  buildE <- getPostBuild
  he <- getFilters ((BTC, 0, 1) <$ buildE)
  performEvent_ $ ffor he $ \h -> liftIO $ print h
  pure ()

getFilters :: MonadFrontBase t m => Event t (Currency, BlockHeight, Int) -> m (Event t [AddrFilter])
getFilters e = pure $ [mockFilter] <$ e 

mockFilter :: AddrFilter
mockFilter = AddrFilterBtc $ either error id $ decodeBtcAddrFilter . hex2bs $ "0000000000000004171bad529ff6142e1d4840"