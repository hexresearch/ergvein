-----------------------------------------------------------------------------
--
-- Module      :  Ergvein.Wallet.Headers.Loader
-- Copyright   :  2019 ATUM SOLUTIONS AG
-- License     :  MIT
--
-- Maintainer  :  Anton Gushcha <ncrashed@protonmail.com>, Vladimir Krutkin <krutkinvs@gmail.com>
-- Stability   :
-- Portability :
--
-- | Contains thread that continuously loads headers from indexers.
--
-----------------------------------------------------------------------------

module Ergvein.Wallet.Headers.Loader (
  headersLoader
) where

import Control.Monad
import Control.Monad.IO.Unlift
import Ergvein.Index.API.Types
import Ergvein.Types.Currency
import Ergvein.Wallet.Client
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Alert

headersLoader :: (HasHeadersStorage m, MonadFrontBase t m, MonadClient t m) => m ()
headersLoader = nameSpace "headers loader" $ do
  sequence_ [headersLoaderBtc]

headersLoaderBtc :: (HasHeadersStorage m, MonadFrontBase t m, MonadClient t m) => m ()
headersLoaderBtc = nameSpace "btc" $ do
  buildE <- getPostBuild
  he <- handleDangerMsg =<< getHeight (HeightRequest BTC <$ buildE)
  performEvent_ $ ffor he $ \h -> liftIO $ print h
  pure ()
