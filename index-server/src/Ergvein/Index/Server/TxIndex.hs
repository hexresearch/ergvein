module Ergvein.Index.Server.TxIndex
  (
    txIndexApp
  , withTxIndexEnv
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Time
import System.DiskSpace
import System.Posix.Signals

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Dependencies
import Ergvein.Index.Server.Metrics
import Ergvein.Index.Server.TxIndex.Monad
import Ergvein.Index.Server.TCPService.Conversions()
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Text.IO as T
import Ergvein.Index.Server.BlockchainScanning.Bitcoin (buildTxIndex, actualHeight)

txIndexApp :: (MonadUnliftIO m, MonadLogger m) => BlockHeight -> Int -> TxIndexEnv -> m ()
txIndexApp btcStartHeight threadNum env = do
  logInfoN $ "Server started at:" <> (showt . cfgServerPort $ envServerConfig env)
  _ <- liftIO $ installHandler sigTERM (Catch onShutdown) Nothing
  _ <- liftIO $ installHandler sigINT  (Catch onShutdown) Nothing
  liftIO $ runTxIndexMIO env $ if threadNum == 1
    then btcTxIndexBuilder btcStartHeight Nothing
    else do
      tipHeight <- actualHeight
      let (q,r) = quotRem (tipHeight - btcStartHeight + 1) $ fromIntegral threadNum
      let num = q + if r /= 0 then 1 else 0
      let bounds = [(n, Just $ n + num - 1) | n <- [btcStartHeight, btcStartHeight + num..tipHeight]]
      let (xs,(lastStart, _)) = (init bounds, last bounds)
      let bounds' = xs ++ [(lastStart, Nothing)]
      void $ mapConcurrently (uncurry btcTxIndexBuilder) bounds'
  where
    onShutdown :: IO ()
    onShutdown = do
      T.putStrLn "Server stop signal recivied..."
      T.putStrLn "service is stopping"
      atomically $ writeTChan (envShutdownChannel env) True
      atomically $ writeTVar (envShutdownFlag env) True

btcTxIndexBuilder :: BlockHeight -> (Maybe BlockHeight) -> TxIndexM ()
btcTxIndexBuilder hbeg mhend = indexIteration hbeg
  where
    blockIteration :: BlockHeight -> BlockHeight -> TxIndexM TxIndexInfo
    blockIteration headBlockHeight blockHeight = do
      now <- liftIO $ getCurrentTime
      let percent = fromIntegral blockHeight / fromIntegral headBlockHeight :: Double
      logInfoN $ "["<> showt (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now) <> "] "
        <> "Building index for <BTC> "
        <> showt blockHeight <> " / " <> showt headBlockHeight <> " (" <> showf 2 (100*percent) <> "%)"
      buildTxIndex blockHeight

    indexIteration :: BlockHeight -> TxIndexM ()
    indexIteration current = do
      shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
      unless shutdownFlag $ do
        headBlockHeight <- actualHeight
        let b = maybe True (current <=) mhend
        when (current <= headBlockHeight && b) $ do
          tryBlockInfo <- try $ blockIteration headBlockHeight current
          enoughSpace <- isEnoughSpace
          flg <- liftIO . readTVarIO =<< getShutdownFlag
          case tryBlockInfo of
            Right (txIndex@TxIndexInfo {..})
              | flg -> logInfoN $ "Told to shut down"
              | enoughSpace && not flg -> do
                addTxIndexInfo BTC txIndex
                indexIteration (succ current)
              | otherwise -> logInfoN $ "Not enough available disc space to store block scan result"
            -- FIXME: Are we swallowing ALL errors here???
            Left (SomeException err) -> do
              logInfoN $ "Error scanning " <> showt current <> " BTC " <> showt err
              indexIteration (current - 1)

isEnoughSpace :: TxIndexM Bool
isEnoughSpace = do
  path <- cfgUtxoDbPath <$> serverConfig
  availSpace <- liftIO $ getAvailSpace path
  setGauge availableSpaceGauge $ fromIntegral (availSpace - requiredAvailSpace)
  pure $ requiredAvailSpace <= availSpace
 where
  requiredAvailSpace = 2^(30::Int) -- 1Gb
