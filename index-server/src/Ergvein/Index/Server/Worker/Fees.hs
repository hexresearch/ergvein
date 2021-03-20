module Ergvein.Index.Server.Worker.Fees
  (
    feesScanner
  ) where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Fixed
import Data.Maybe
import Network.Bitcoin.Api.Misc

-- import Ergvein.Index.Server.BlockchainScanning.Bitcoin
import Ergvein.Index.Server.Bitcoin.API
import Ergvein.Index.Server.Config
-- import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.Utils
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Transaction

import qualified Ergvein.Index.Protocol.Types as IPT

feesScanner :: ServerMonad m => m [Thread]
feesScanner = sequenceA
  [ feesThread btcFeeScaner
  ]

feesThread :: ServerMonad m => m () -> m Thread
feesThread feescan = create $ logOnException "feesThread" . \thread -> do
  feescan
  stopThreadIfShutdown thread

btcFeeScaner :: ServerMonad m => m ()
btcFeeScaner = feeScaner' 0
  where
    feeScaner' :: ServerMonad m => BlockHeight -> m ()
    feeScaner' h = do
      cfg <- serverConfig
      h'  <- fmap fromIntegral actualHeight
      h'' <- if h' == h
        then pure h'
        else do
          res <- fmap catMaybes $ flip traverse [FeeFast, FeeModerate, FeeCheap] $ \lvl -> do
            let req mode c = estimateSmartFee c (fromIntegral $ feeTargetBlocks BTC lvl) mode
            mco <- nodeRpcCall $ req Conservative
            mec <- nodeRpcCall $ req Economical
            case (estimateResFee mco, estimateResFee mec) of
              (Just (MkFixed co), Just (MkFixed ec)) -> pure $ Just (lvl, (fromIntegral co `div` 1000 , fromIntegral ec `div` 1000))
              _ -> pure Nothing
          let isTestnet = cfgBTCNodeIsTestnet cfg
              currencyCode = if isTestnet then IPT.TBTC else IPT.BTC
          setFees currencyCode $ mkFeeBundle res
          logInfoN $ "[BTC]: " <> showt res
          pure $ case res of
            [] -> h
            _  -> h'
      shutdownFlagVar <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlagVar $ cfgFeeEstimateDelay cfg
      shutdownFlag <- liftIO $ readTVarIO shutdownFlagVar
      unless shutdownFlag $ feeScaner' h''
