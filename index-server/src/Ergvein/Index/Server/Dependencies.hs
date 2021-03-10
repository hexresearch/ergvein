module Ergvein.Index.Server.Dependencies
 ( HasBitcoinNodeNetwork(..)
 , HasDiscoveryRequisites(..)
 , HasShutdownSignal(..)
 , MonadFees(..)
 , HasConnectionsManagement(..)
 , HasBroadcastChannel(..)
 -- * Utils
 , stopThreadIfShutdown
 , interruptThreadOnShutdown
 , broadcastSocketMessage
 , logOnException
 )where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.Function
import Data.Map.Strict (Map)
import Data.Text(Text)
import Data.Typeable
import Network.Socket

import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Text
import Ergvein.Types.Fees

import qualified Data.Map.Strict            as Map
import qualified Network.Haskoin.Constants  as HK

class HasBitcoinNodeNetwork m where
  currentBitcoinNetwork :: m HK.Network

class HasDiscoveryRequisites m where
  getDiscoveryRequisites  :: m PeerDiscoveryRequisites

class HasShutdownSignal m where
  getShutdownFlag  :: m (TVar Bool)
  getShutdownChannel :: m (TChan Bool)

class MonadFees m where
  getFees :: m (Map.Map CurrencyCode FeeBundle)
  setFees :: CurrencyCode -> FeeBundle -> m ()

class MonadUnliftIO m => HasConnectionsManagement m where
  openConnections :: m (TVar (Map SockAddr (ThreadId, Socket)))

class MonadUnliftIO m => HasBroadcastChannel m where
  broadcastChannel :: m (TChan Message)

-- Utils

stopThreadIfShutdown :: (HasShutdownSignal m, MonadIO m) => Thread -> m ()
stopThreadIfShutdown thread = do
  shutdownFlag <- liftIO . readTVarIO =<< getShutdownFlag
  when shutdownFlag $ liftIO $ stop thread

interruptThreadOnShutdown :: (HasShutdownSignal m, MonadIO m) => Thread -> m ()
interruptThreadOnShutdown thread = do
  shutChan <- liftIO . atomically . cloneTChan =<< getShutdownChannel
  liftIO $ fix $ \next -> do
    shutdownFlag <- atomically $ readTChan shutChan
    if shutdownFlag then stop thread else next

broadcastSocketMessage :: HasBroadcastChannel m => Message -> m ()
broadcastSocketMessage msg = liftIO . atomically . flip writeTChan msg =<< broadcastChannel

-- | Log exceptions at Error severity
logOnException :: (
    MonadIO m
  , MonadLogger m
  , MonadCatch m)
  => Text -> m a -> m a
logOnException threadName = handle logE
  where
    logInfoN' :: MonadLogger m => Text -> m ()
    logInfoN' t = logInfoN $ "[" <> threadName <> "]: " <> t
    logErrorN' :: MonadLogger m => Text -> m ()
    logErrorN' t = logErrorN $ "[" <> threadName <> "]: " <> t
    logE :: (
        MonadIO m
      , MonadLogger m
      , MonadCatch m) => SomeException -> m b
    logE e
        | Just ThreadKilled <- fromException e = do
            logInfoN' "Killed normally by ThreadKilled"
            throwM e
        | Just (ioe :: IOException) <- fromException e = do
            logErrorN' $ "Killed by IOException. " <> showt ioe
            liftIO $ threadDelay 1000000
            throwM e
        | SomeException eTy <- e = do
            logErrorN' $ "Killed by " <> showt (typeOf eTy) <> ". " <> showt eTy
            liftIO $ threadDelay 1000000
            throwM e
