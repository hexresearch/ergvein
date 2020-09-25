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
    blockIteration headBlockHeight blockHeight = do
      now <- liftIO $ getCurrentTime
      let percent = fromIntegral blockHeight / fromIntegral headBlockHeight :: Double
      logInfoN $ "["<> showt (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now) <> "] "
        <> "Scanning height for " <> showt currency <> " "
        <> showt blockHeight <> " / " <> showt headBlockHeight <> " (" <> showf 2 (100*percent) <> "%)"
        -- <> showt (length $ spentTxsHash bi)
      scanInfo blockHeight

    scanIteration :: Thread -> ServerM ()
    scanIteration thread = do
      cfg <- serverConfig
      scanned <- getScannedHeight currency
      let toScanFrom = maybe (currencyHeightStart currency) succ scanned
      go toScanFrom
      shutdownFlag <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlag $ cfgBlockchainScanDelay cfg
      stopThreadIfShutdown thread
      where
        go :: BlockHeight -> ServerM ()
        go current = do
          shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
          unless shutdownFlag $ do
            headBlockHeight <- actualHeight currency
            when (current <= headBlockHeight) $ do
              tryBlockInfo <- (Right <$> blockIteration headBlockHeight current) `catch` (\(SomeException ex) -> pure $ Left $ show ex)
              enoughSpace <- isEnoughSpace
              case tryBlockInfo of
                Right (blockInfo@BlockInfo {..}) | enoughSpace -> do
                  previousBlockSame <- isPreviousBlockSame $ blockMetaPreviousHeaderBlockHash $ blockInfoMeta
                  if previousBlockSame then do --fork detection
                    addBlockInfo blockInfo
                    when (current == headBlockHeight) $ broadcastFilter $ blockInfoMeta
                    go (succ current)
                  else previousBlockChanged current
                _ | not enoughSpace ->
                  logInfoN $ "Not enough available disc space to store block scan result"
                Left errMsg -> blockScanningError errMsg current

        isPreviousBlockSame proposedPreviousBlockId = do
          maybeLastScannedBlock <- getLastScannedBlock currency
          pure $ flip all maybeLastScannedBlock (== proposedPreviousBlockId)

        previousBlockChanged from = do
          revertedBlocksCount <- fromIntegral <$> performRollback currency
          logInfoN $ "Fork detected at "
                  <> showt from <> " " <> showt currency
                  <> ", performing rollback of " <> showt revertedBlocksCount <> " previous blocks"
          let restart = (from - revertedBlocksCount)
          setScannedHeight currency restart
          go restart

        blockScanningError errorMessage from = do
          logInfoN $ "Error scanning " <> showt from <> " " <> showt currency <> " " <> T.pack errorMessage
          previousBlockChanged from

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
