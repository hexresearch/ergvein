module Ergvein.Index.Server.Worker.Rates
  (
    ratesScanner
  ) where

import Binance.Client
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Logger

import Ergvein.Text
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad

import qualified Data.Map.Strict as M

ratesScanner :: ServerM Thread
ratesScanner = create $ logOnException "ratesScanner" . \thread -> do
  void $ fork $ interruptThreadOnShutdown thread
  ratesThread

ratesThread :: ServerM ()
ratesThread = do
  dt <- fmap cfgRatesRefreshPeriod $ asks envServerConfig
  xratesVar <- asks envExchangeRates
  forever $ do
    usdt <- getCurrentPrice BTCUSDT
    busd <- getCurrentPrice BTCBUSD
    let m = M.fromList [(BTCUSDT, usdt), (BTCBUSD, busd)]
    logInfoN $ "Rates: " <> showt m
    liftIO $ atomically $ modifyTVar xratesVar $ M.union m
    liftIO $ threadDelay dt
