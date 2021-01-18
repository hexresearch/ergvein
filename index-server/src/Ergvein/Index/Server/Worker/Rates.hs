module Ergvein.Index.Server.Worker.Rates
  (
    ratesScanner
  ) where

import Binance.Client
import Control.Concurrent
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Reader

import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad

import qualified Data.Map.Strict as M

ratesScanner :: ServerM Thread
ratesScanner = create $ logOnException "ratesScanner" . \thread -> do
  ratesThread
  stopThreadIfShutdown thread

ratesThread :: ServerM ()
ratesThread = do
  dt <- fmap cfgRatesRefreshPeriod $ asks envServerConfig
  xratesVar <- asks envExchangeRates
  forever $ liftIO $ do
    usdt <- getCurrentPrice BTCUSDT
    busd <- getCurrentPrice BTCBUSD
    let m = M.fromList [(BTCUSDT, usdt), (BTCBUSD, busd)]
    atomically $ modifyTVar xratesVar $ M.union m
    threadDelay dt
