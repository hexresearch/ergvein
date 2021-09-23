module Ergvein.Core.Worker.Store(
    storeWorker
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Time
import Ergvein.Core.Store
import Ergvein.Types.WalletInfo
import Reflex.Fork
import Sepulcas.Native
import System.Directory

import qualified Data.List as L
import qualified Data.Text as T

storeWorker :: MonadStorage t m => m ()
storeWorker = do
  storeDir <- getStoreDir
  storeMutex <- getStoreMutex
  storeChan <- getStoreChan
  liftIO $ walletStoreThread storeDir storeMutex storeChan
  when isAndroid (deleteTmpFiles storeDir)

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
walletStoreThread :: PlatformNatives => Text -> MVar () -> TChan StoreWalletMsg -> IO ()
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
          Just storeWalletMsg@StoreWalletMsg{..} -> do
            currTime <- readTVar timeRef
            updTime <- readTVar lastUpdTimeRef
            when ((diffUTCTime currTime updTime < storeTimeBetweenWrites) && (storeWalletMsg'priority /= StoreWalletPriorityHigh)) retry
            writeTVar lastUpdTimeRef currTime
            writeTVar lastStoreRef Nothing
            pure storeWalletMsg
  -- Thread that indefinetely queries if we need to write down new state
  fix $ \next -> do
    storeWalletMsg <- atomically getTimedWrite
    case storeWalletMsg'closeStoreWorker storeWalletMsg of
      False -> do
        storeWalletIO (storeWalletMsg'caller storeWalletMsg) storeDir mutex (storeWalletMsg'walletInfo storeWalletMsg)
        next
      True -> pure ()

storeWalletIO :: PlatformNatives => Text -> Text -> MVar () -> WalletInfo -> IO ()
storeWalletIO caller storeDir mutex ai = do
  let storage = _walletInfo'storage ai
  let eciesPubKey = _walletInfo'eciesPubKey ai
  withMVar mutex $ const $ flip runReaderT storeDir $ saveStorageToFile caller eciesPubKey storage
