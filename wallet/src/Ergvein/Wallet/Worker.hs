{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Worker (
    spawnWorkers
  ) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Control.Monad.Reader
import Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.Directory

import Ergvein.Types.AuthInfo
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Sepulcas.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Worker.Fees
import Ergvein.Wallet.Worker.Height
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Worker.PubKeysGenerator
import Ergvein.Wallet.Worker.Rates

import qualified Data.List as L

-- Workers and other routines go here
spawnWorkers :: MonadFront t m => m ()
spawnWorkers = do
  storeDir <- getStoreDir
  storeMutex <- getStoreMutex
  storeChan <- getStoreChan
  liftIO $ walletStoreThread storeDir storeMutex storeChan
  when isAndroid (deleteTmpFiles storeDir)
  -- initFiltersHeights filtersHeights
  scanner
  btcNodeController
  ratesWorker
  heightAsking
  feesWorker
  pubKeysGenerator
  pure ()

-- Deletes files created with 'atomicWriteFile' from specified directiry
deleteTmpFiles :: MonadIO m => Text -> m ()
deleteTmpFiles dir = liftIO $ do
  entries <- listDirectory $ T.unpack dir
  traverse_ removeFile $ L.filter isTmpFile entries
  where isTmpFile filePath = "atomic" `L.isPrefixOf` filePath && ".write" `L.isSuffixOf` filePath

-- | Minimum time between two writes of storage to disk
storeTimeBetweenWrites :: NominalDiffTime
storeTimeBetweenWrites = 20

-- | Thread that writes down updates of wallet storages and checks that write down doesn't occur too frequent.
walletStoreThread :: PlatformNatives => Text -> MVar () -> TChan (Text, AuthInfo) -> IO ()
walletStoreThread storeDir mutex updChan = void $ forkOnOther $ do
  timeRef <- newTVarIO =<< getCurrentTime
  lastUpdTimeRef <- newTVarIO =<< getCurrentTime
  lastStoreRef <- newTVarIO Nothing
  -- Thread that updates reference with time to compare it with value in lastUpdTimeRef in getTimedWrite
  void $ forkIO $ fix $ \next -> do
    threadDelay $ 1000000 * ceiling storeTimeBetweenWrites
    currTime <- getCurrentTime
    atomically $ writeTVar timeRef currTime
    next
  -- Thread that reads from chan and stores last storage to reference which next thread will check and validate
  -- against timeout.
  void $ forkIO $ fix $ \next -> do
    atomically $ do
      val <- readTChan updChan
      writeTVar lastStoreRef $ Just val
    next
  -- If we have awaiting write to disk and time passed > timeout we return the value unless retry
  let getTimedWrite = do
        mval <- readTVar lastStoreRef
        case mval of
          Nothing -> retry
          Just val -> do
            currTime <- readTVar timeRef
            updTime <- readTVar lastUpdTimeRef
            when (diffUTCTime currTime updTime < storeTimeBetweenWrites) retry
            writeTVar lastUpdTimeRef currTime
            writeTVar lastStoreRef Nothing
            pure val
  -- Thread that indefinetely queries if we need to write down new state
  fix $ \next -> do
    (caller, authInfo) <- atomically getTimedWrite
    storeWalletIO caller storeDir mutex authInfo
    next

storeWalletIO :: PlatformNatives => Text -> Text -> MVar () -> AuthInfo -> IO ()
storeWalletIO caller storeDir mutex ai = do
  let storage = _authInfo'storage ai
  let eciesPubKey = _authInfo'eciesPubKey ai
  withMVar mutex $ const $ flip runReaderT storeDir $ saveStorageToFile caller eciesPubKey storage
