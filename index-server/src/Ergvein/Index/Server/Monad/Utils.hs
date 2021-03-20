module Ergvein.Index.Server.Monad.Utils
  (
    logOnException
  , stopThreadIfShutdown
  , interruptThreadOnShutdown
  , broadcastSocketMessage
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Immortal
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Text (Text)
import Data.Typeable

import Ergvein.Index.Protocol.Types (Message)
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Monad.Class
import Ergvein.Text

-- | Log exceptions at Error severity
logOnException :: (
    HasServerConfig m
  , MonadIO m
  , MonadLogger m
  , MonadCatch m)
  => Text -> m a -> m a
logOnException threadName = handle logE
  where
    logE :: (
        HasServerConfig m
      , MonadIO m
      , MonadLogger m
      , MonadCatch m) => SomeException -> m b
    logE e
        | Just ThreadKilled <- fromException e = do
            logInfoN $ "[" <> threadName <> "]: Killed normally by ThreadKilled"
            throwM e
        | SomeException eTy <- e = do
            logErrorN $ "[" <> threadName <> "]: Killed by " <> showt (typeOf eTy) <> ". " <> showt eTy
            liftIO $ threadDelay 1000000
            throwM e

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
