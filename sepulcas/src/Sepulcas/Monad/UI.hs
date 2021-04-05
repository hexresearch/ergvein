module Sepulcas.Monad.UI where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Reflex
import Reflex.Fork

class MonadIO m => MonadHasUI m where
  -- | Internal method of getting channel where you can post actions that must be
  -- executed in main UI thread.
  getUiChan :: m (Chan (IO ()))

instance MonadIO m => MonadHasUI (ReaderT (Chan (IO ())) m) where
  getUiChan = ask
  {-# INLINE getUiChan #-}

instance MonadIO m => MonadHasUI (ReaderT (Chan (IO ()), a) m) where
  getUiChan = asks fst
  {-# INLINE getUiChan #-}

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread :: (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), MonadHasUI m) => Event t (Performable m a) -> m (Event t a)
runOnUiThread ema = do
  ch <- getUiChan
  performEventAsync $ ffor ema $ \ma fire -> do
    unlift <- askUnliftIO
    liftIO $ writeChan ch $ fire =<< unliftIO unlift ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread_ :: (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), MonadHasUI m) => Event t (Performable m ()) -> m ()
runOnUiThread_ ema = do
  ch <- getUiChan
  performEvent_ $ ffor ema $ \ma -> do
    unlift <- askUnliftIO
    liftIO $ writeChan ch (unliftIO unlift ma)

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadA :: MonadHasUI m => IO a -> m (Async a)
runOnUiThreadA ma = do
  ch <- getUiChan
  liftIO $ do
    resVar <- newEmptyMVar
    writeChan ch $ putMVar resVar =<< ma
    async $ takeMVar resVar

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadM :: MonadHasUI m => IO () -> m ()
runOnUiThreadM ma = do
  ch <- getUiChan
  liftIO $ writeChan ch ma
