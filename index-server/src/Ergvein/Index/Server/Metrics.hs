module Ergvein.Index.Server.Metrics(
    activeConnsGauge
  , filtersServedCounter
  -- * Helpers
  , incGaugeWhile
  -- * Metrics server
  , serveMetrics
  -- * Reexports
  , addGauge
  , subGauge
  , setGauge
  , incCounter
  , addCounter
  ) where

import Control.Immortal.Worker (worker)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Catch
import Control.Monad.Logger
import Data.Functor (void)
import Data.String (fromString)
import Data.Text (pack)
import Ergvein.Index.Server.Config
import Ergvein.Text
import Network.HTTP.Types.Status (status401)
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost)
import Network.Wai.Middleware.Prometheus (prometheus, def, PrometheusSettings(..))
import Prometheus (register, unsafeRegister, Counter, counter, incCounter, addCounter, Gauge, gauge, Info(..), addGauge, subGauge, setGauge, MonadMonitor)
import Prometheus.Metric.GHC (ghcMetrics)

activeConnsGauge :: Gauge
activeConnsGauge = unsafeRegister $ gauge (Info "active_connections" "Amount of opened TCP connections")
{-# NOINLINE activeConnsGauge #-}

filtersServedCounter :: Counter
filtersServedCounter = unsafeRegister $ counter (Info "filters_served" "Amount of served filters")
{-# NOINLINE filtersServedCounter #-}

incGaugeWhile :: (MonadMonitor m, MonadMask m) => Gauge -> m a -> m a
incGaugeWhile g = bracket_ (addGauge activeConnsGauge 1.0) (subGauge activeConnsGauge 1.0)

-- | Start server with prometheus metrics if corresponding config section is defined.
--
-- To receive ghc metrics the server should be start with "+RTS -T" flags.
serveMetrics :: (HasServerConfig m, MonadLogger m, MonadUnliftIO m) => m ()
serveMetrics = void $ worker "metrics-server" $ const $ do
  cfg <- serverConfig
  case cfgMetrics cfg of
    Nothing -> logInfoN "No metrics server started"
    Just CfgMetrics{..} -> do
      logInfoN $ "Metrics server is started at " <> pack cfgMetricsHost <> ":" <> showt cfgMetricsPort
      let sett = setPort cfgMetricsPort $ setHost (fromString cfgMetricsHost) defaultSettings
          pcfg = def { prometheusEndPoint = [] }
      register ghcMetrics
      liftIO $ runSettings sett $ prometheus pcfg noApp

-- | App that serves 404 on any page
noApp :: Application
noApp _ respond = respond $ responseLBS status401 [] "Not found"
