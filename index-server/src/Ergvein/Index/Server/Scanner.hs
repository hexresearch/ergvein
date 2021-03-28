module Ergvein.Index.Server.Scanner
  (
    scanningInfo
  , blockchainScanners
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Immortal
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Time
import System.DiskSpace

import Ergvein.Index.Protocol.Types (Message(..), FilterEvent(..))
import Ergvein.Index.Server.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.Monad
import Ergvein.Index.Server.TCPService.Conversions
import Ergvein.Index.Server.TCPService.Conversions()
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Ergvein.Index.Server.Bitcoin.API as BtcApi
import qualified Ergvein.Index.Server.Bitcoin.Scanner as BTCScanner

actualHeight :: (BtcApi.BitcoinApiMonad m, Monad m) => Currency -> m BlockHeight
actualHeight currency = case currency of
  BTC -> fromIntegral <$> BtcApi.actualHeight
  ERGO -> pure 0

scanningInfo :: (HasDbs m, MonadCatch m, BtcApi.BitcoinApiMonad m) => m [ScanProgressInfo]
scanningInfo = catMaybes <$> mapM nfo allCurrencies
  where
    nfo :: (HasDbs m, MonadCatch m, BtcApi.BitcoinApiMonad m) => Currency -> m (Maybe ScanProgressInfo)
    nfo currency = do
      maybeScanned <- getScannedHeight currency
      maybeActual <- (Just <$> actualHeight currency) `catch` (\(SomeException _) -> pure Nothing)
      pure $ ScanProgressInfo currency <$> maybeScanned <*> maybeActual

runBtcScanner :: ServerMonad m => m Thread
runBtcScanner = create btcScanner
{-# INLINE runBtcScanner #-}

btcScanner :: forall m . ServerMonad m => Thread -> m ()
btcScanner thread = logOnException threadName $ do
  scanned <- getScannedHeight BTC
  let toScanFrom = maybe (currencyHeightStart BTC) succ scanned
  go toScanFrom
  where
    threadName = "scannerThread<BTC>"
    logInfoNamed t = logInfoN $ "[" <> threadName <> "]: " <> t
    logAndShutdown t = logInfoNamed t >> (liftIO $ stop thread)

    printInfo :: BlockHeight -> BlockHeight -> m ()
    printInfo headBlockHeight blockHeight = do
      now <- liftIO $ getCurrentTime
      let percent = fromIntegral blockHeight / fromIntegral headBlockHeight :: Double
      logInfoN $ "["<> showt (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now)
        <> "] Scanning height: "
        <> showt blockHeight <> " / " <> showt headBlockHeight <> " (" <> showf 2 (100*percent) <> "%)"

    go :: BlockHeight -> m ()
    go current = do
      headBlockHeight <- actualHeight BTC
      reportCurrentHeight BTC headBlockHeight
      shutdownChan <- getShutdownChannel
      shutdownFlagVar <- getShutdownFlag
      if (current <= headBlockHeight) then do
        printInfo headBlockHeight current
        eBlockInfo <- try $ BTCScanner.scanBlock current
        case eBlockInfo of
          Right blockInfo -> do
            previousBlockSame <- isPreviousBlockSame $ blockMetaPreviousHeaderBlockHash $ blockInfoMeta blockInfo
            shutdownFlag <- liftIO . readTVarIO $ shutdownFlagVar
            if shutdownFlag then logAndShutdown "Shutdown"
            else if previousBlockSame then do
              es <- isEnoughSpace
              if not es then logAndShutdown "Error! Not enough space"
              else do
                commitBlockInfo blockInfo
                when (current == headBlockHeight) $ broadcastFilter $ blockInfoMeta blockInfo
                reportScannedHeight BTC current
                go (succ current)
            else previousBlockChanged current
          Left (SomeException err) ->
            logAndShutdown $ "Error scanning " <> showt current <> ": " <> showt err
      else do
        shutdownFlag <- liftIO . readTVarIO $ shutdownFlagVar
        if shutdownFlag then logAndShutdown "Shutdown"
        else do
          evnt <- liftIO $ race (threadDelay 3000000) (atomically $ readTChan shutdownChan)
          case evnt of
            Left _  -> go current
            Right _ -> logAndShutdown "Received shutdown signal"

    isPreviousBlockSame proposedPreviousBlockId = do
      maybeLastScannedBlock <- getLastScannedBlock BTC
      pure $ flip all maybeLastScannedBlock (== proposedPreviousBlockId)

    previousBlockChanged from = do
      revertedBlocksCount <- fromIntegral <$> performRollback BTC
      logInfoNamed $ "Fork detected at "
        <> showt from
        <> ", performing rollback of "
        <> showt revertedBlocksCount <> " previous blocks"
      let restart = (from - revertedBlocksCount)
      setScannedHeight BTC restart
      go restart

broadcastFilter :: ServerMonad m => BlockMeta -> m ()
broadcastFilter BlockMeta{..} = do
  currencyCode <- currencyToCurrencyCode blockMetaCurrency
  broadcastSocketMessage $ MFiltersEvent $ FilterEvent
    { filterEventCurrency     = currencyCode
    , filterEventHeight       = blockMetaBlockHeight
    , filterEventBlockId      = blockMetaHeaderHash
    , filterEventBlockFilter  = blockMetaAddressFilter
    }

blockchainScanners :: ServerMonad m => m [Thread]
blockchainScanners = sequenceA [ runBtcScanner ]

isEnoughSpace :: ServerMonad m => m Bool
isEnoughSpace = do
  path <- cfgDbPath <$> serverConfig
  availSpace <- liftIO $ getAvailSpace path
  setGauge availableSpaceGauge $ fromIntegral (availSpace - requiredAvailSpace)
  pure $ requiredAvailSpace <= availSpace
 where
  requiredAvailSpace = 2^(30::Int) -- 1Gb
