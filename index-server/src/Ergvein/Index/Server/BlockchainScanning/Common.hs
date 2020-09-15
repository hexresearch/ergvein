module Ergvein.Index.Server.BlockchainScanning.Common where

import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Conversion
import Data.Maybe
import Data.Time
import System.DiskSpace

import Ergvein.Index.Protocol.Types (Message(..), FilterEvent(..))
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions()
import Ergvein.Index.Server.Utils
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Text as T
import qualified Ergvein.Index.Server.BlockchainScanning.Bitcoin as BTCScanning
import qualified Ergvein.Index.Server.BlockchainScanning.Ergo    as ERGOScanning

scanningInfo :: ServerM [ScanProgressInfo]
scanningInfo = catMaybes <$> mapM nfo allCurrencies
  where
    nfo :: Currency -> ServerM (Maybe ScanProgressInfo)
    nfo currency = do
      maybeScanned <- getScannedHeight currency
      maybeActual <- (Just <$> actualHeight currency) `catch` (\(SomeException _) -> pure Nothing)
      pure $ ScanProgressInfo currency <$> maybeScanned <*> maybeActual

actualHeight :: Currency -> ServerM BlockHeight
actualHeight currency = case currency of
  BTC  -> BTCScanning.actualHeight
  ERGO -> ERGOScanning.actualHeight

scannerThread :: Currency -> (BlockHeight -> ServerM BlockInfo) -> ServerM Thread
scannerThread currency scanInfo = create $ logOnException threadName . scanIteration
  where
    threadName = "scannerThread<" <> showt currency <> ">"
    blockIteration :: BlockHeight -> BlockHeight -> ServerM BlockInfo
    blockIteration totalh blockHeight = do
      now <- liftIO $ getCurrentTime
      let percent = fromIntegral blockHeight / fromIntegral totalh :: Double
      logInfoN $ "["<> showt now <> "] "
        <> "Scanning height for " <> showt currency <> " "
        <> showt blockHeight <> " / " <> showt totalh <> " (" <> showf 2 (100*percent) <> "%)"
        -- <> showt (length $ spentTxsHash bi)
      scanInfo blockHeight

    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      logInfoN "Start scan iteration"
      cfg <- serverConfig
      actual  <- actualHeight currency
      scanned <- getScannedHeight currency
      let toScanFrom = maybe (currencyHeightStart currency) succ scanned
      go toScanFrom actual
      shutdownFlag <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlag $ cfgBlockchainScanDelay cfg
      stopThreadIfShutdown thread
      where
        go current to = do
          shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
          when (not shutdownFlag && current <= to) $ do
            tryBlockInfo <- (Right <$> blockIteration to current) `catch` (\(SomeException ex) -> pure $ Left $ show ex)
            enoughSpace <- isEnoughSpace
            case tryBlockInfo of
              Right blockInfo | enoughSpace -> do
                previousBlockSame <- isPreviousBlockSame $ blockMetaPreviousHeaderBlockHash $ blockInfoMeta blockInfo
                if previousBlockSame then do --fork detection
                  addBlockInfo blockInfo to
                  when (current == to) $ broadcastFilter $ blockInfoMeta blockInfo
                  go (succ current) to
                else previousBlockChanged current to

              _ | not enoughSpace ->
                logInfoN $ "Not enough available disc space to store block scan result"

              Left errMsg -> blockScanningError errMsg current to

        isPreviousBlockSame proposedPreviousBlockId = do
          maybeLastScannedBlock <- getLastScannedBlock currency
          pure $ flip all maybeLastScannedBlock (== proposedPreviousBlockId)

        previousBlockChanged from to = do
          revertedBlocksCount <- fromIntegral <$> performRollback currency
          logInfoN $ "Fork detected at "
                  <> showt from <> " " <> showt currency
                  <> ", performing rollback of " <> showt revertedBlocksCount <> " previous blocks"
          let restart = (from - revertedBlocksCount)
          setScannedHeight currency restart
          go restart to

        blockScanningError errorMessage from to = do
          logInfoN $ "Error scanning " <> showt from <> " " <> showt currency <> " " <> T.pack errorMessage
          previousBlockChanged from to

broadcastFilter :: BlockMetaInfo -> ServerM ()
broadcastFilter BlockMetaInfo{..} =
  broadcastSocketMessage $ MFiltersEvent $ FilterEvent
  { filterEventCurrency     = convert blockMetaCurrency
  , filterEventHeight       = blockMetaBlockHeight
  , filterEventBlockId      = blockMetaHeaderHash
  , filterEventBlockFilter  = blockMetaAddressFilter
  }

blockchainScanning :: ServerM [Thread]
blockchainScanning = sequenceA
  [ scannerThread BTC  BTCScanning.blockInfo
  ]

feesThread :: ServerM () -> ServerM Thread
feesThread feescan = create $ logOnException "feesThread" . \thread -> do
  feescan
  stopThreadIfShutdown thread

isEnoughSpace :: ServerM Bool
isEnoughSpace = do
  path <- cfgFiltersDbPath <$> serverConfig
  availSpace <- liftIO $ getAvailSpace path
  pure $ requiredAvailSpace <= availSpace
 where
  requiredAvailSpace = 2^30 -- 1Gb

feesScanning :: ServerM [Thread]
feesScanning = sequenceA
  [ feesThread BTCScanning.feeScaner
  ]
