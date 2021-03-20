module Ergvein.Index.Server.Worker.Rates
  (
    ratesScanner
  ) where

import Coinbase.Client
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Reader
import Control.Monad.Logger

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Monad.Impl

import qualified Data.Map.Strict as M
import qualified Ergvein.Index.Protocol.Types as IPT

ratesScanner :: ServerM Thread
ratesScanner = create $ logOnException "ratesScanner" . \thread -> do
  void $ fork $ interruptThreadOnShutdown thread
  ratesThread

ratesThread :: ServerM ()
ratesThread = do
  isTestnet <- fmap cfgBTCNodeIsTestnet $ asks envServerConfig
  let btcCC = currencyToCurrencyCode isTestnet BTC
  dt <- fmap cfgRatesRefreshPeriod $ asks envServerConfig
  xratesVar <- asks envExchangeRates
  forever $ do
    ratesBtc <- coinbaseReqMultipleRates BTC [USD, RUB, EUR]
    liftIO $ atomically $ modifyTVar xratesVar $ M.insert btcCC (realToFrac <$> ratesBtc)
    logInfoN $ "Rates: " <> showt ratesBtc
    liftIO $ threadDelay dt

currencyToCurrencyCode :: Bool -> Currency -> IPT.CurrencyCode
currencyToCurrencyCode isTestnet c = case c of
  BTC -> if isTestnet then IPT.TBTC else IPT.BTC
  ERGO -> if isTestnet then IPT.TERGO else IPT.ERGO
