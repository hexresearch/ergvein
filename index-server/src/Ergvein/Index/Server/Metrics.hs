module Ergvein.Index.Server.Metrics(
    serveMetrics
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text (pack)
import Ergvein.Index.Server.Config
import Ergvein.Text

-- | Start server with prometheus metrics if corresponding config section is defined
serveMetrics :: (HasServerConfig m, MonadLogger m, MonadIO m) => m ()
serveMetrics = do
  cfg <- serverConfig
  case cfgMetrics cfg of
    Nothing -> logInfoN "No metrics server started"
    Just CfgMetrics{..} -> do
      logInfoN $ "Metrics server is started at " <> pack cfgMetricsHost <> ":" <> showt cfgMetricsPort
