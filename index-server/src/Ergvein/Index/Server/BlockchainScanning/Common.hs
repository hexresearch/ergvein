module Ergvein.Index.Server.BlockchainScanning.Common where

import Control.Concurrent
import Control.Immortal
import Control.Monad.Catch
import Control.Monad.Logger
import Data.Foldable (traverse_)
import Data.Maybe

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Text
import Control.Monad.Reader
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.DB.Queries

import qualified Network.Bitcoin.Api.Client                      as BitcoinApi
import qualified Ergvein.Index.Server.BlockchainScanning.Bitcoin as BTCScanning
import qualified Ergvein.Index.Server.BlockchainScanning.Ergo    as ERGOScanning
import Database.Esqueleto

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !(Maybe BlockHeight)
  , nfoActualHeight  :: !BlockHeight
  }

scanningInfo :: ServerM [ScanProgressInfo]
scanningInfo = mapM nfo allCurrencies
  where
    nfo :: Currency -> ServerM ScanProgressInfo
    nfo currency = do
      maybeScanned <- getScannedHeightCache currency
      actual <- actualHeight currency
      pure $ ScanProgressInfo currency maybeScanned actual

blockHeightsToScan :: Currency -> ServerM [BlockHeight]
blockHeightsToScan currency = do
  actual  <- actualHeight currency
  scanned <- getScannedHeightCache currency
  
  let start = maybe (currencyHeightStart currency) succ scanned
  pure [start..actual]

actualHeight :: Currency -> ServerM BlockHeight
actualHeight currency = case currency of
  BTC  -> BTCScanning.actualHeight
  ERGO -> ERGOScanning.actualHeight 

scannerThread :: Currency -> (BlockHeight -> ServerM BlockInfo) -> ServerM Thread
scannerThread currency scanInfo = create $ logOnException . scanIteration
  where
    blockIteration :: BlockHeight -> BlockHeight -> ServerM ()
    blockIteration totalh blockHeight = do
      let percent = fromIntegral blockHeight / fromIntegral totalh :: Double
      logInfoN $ "Scanning height for " <> showt currency <> " " <> showt blockHeight <> " (" <> showf 2 (100*percent) <> "%)"
      do
        blockInfo <- scanInfo blockHeight
        addToCache blockInfo

    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      cfg <- serverConfig
      totalh <-  actualHeight currency
      heights <- blockHeightsToScan currency
      traverse_ (blockIteration totalh) heights
      liftIO $ threadDelay $ cfgBlockchainScanDelay cfg

blockchainScanning :: ServerM [Thread]
blockchainScanning = sequenceA
  [ scannerThread BTC  BTCScanning.blockInfo
  , scannerThread ERGO ERGOScanning.blockInfo
  ]

feesThread :: ServerM () -> ServerM Thread
feesThread feescan = create $ logOnException . const feescan

feesScanning :: ServerM [Thread]
feesScanning = sequenceA
  [ feesThread BTCScanning.feeScaner
  ]
