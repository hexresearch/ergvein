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

import Data.Foldable (traverse_)
import Control.Concurrent
import Control.Immortal
import Control.Monad
import Control.Monad.IO.Unlift
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Monad.Util

headersLoader :: (HasHeadersStorage m, MonadUnliftIO m, PlatformNatives) => m Thread
headersLoader = createWithLabel "headers loader" $ \thread -> do
  ts <- sequence [headersLoaderBtc]
  let stopThem _ = traverse_ (liftIO . stop) ts
  onFinish stopThem $ liftIO $ forever $ threadDelay 1000000

headersLoaderBtc :: (HasHeadersStorage m, MonadUnliftIO m, PlatformNatives) => m Thread
headersLoaderBtc = worker "headers loader btc" $ const $ forever $ do

  pure ()




