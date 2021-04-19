module Reflex.Main.Thread where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Reflex
import Reflex.Fork

class MonadIO m => MonadHasMain m where
  -- | Internal method of getting channel where you can post actions that must be
  -- executed in main UI thread.
  getMainThreadChan :: m (Chan (IO ()))

instance MonadIO m => MonadHasMain (ReaderT (Chan (IO ())) m) where
  getMainThreadChan = ask
  {-# INLINE getMainThreadChan #-}

instance MonadIO m => MonadHasMain (ReaderT (Chan (IO ()), a) m) where
  getMainThreadChan = asks fst
  {-# INLINE getMainThreadChan #-}

type PerformMain t m = (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), MonadHasMain m)

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnMainThread :: PerformMain t m => Event t (Performable m a) -> m (Event t a)
runOnMainThread ema = do
  ch <- getMainThreadChan
  performEventAsync $ ffor ema $ \ma fire -> do
    unlift <- askUnliftIO
    liftIO $ writeChan ch $ fire =<< unliftIO unlift ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnMainThread_ :: PerformMain t m => Event t (Performable m ()) -> m ()
runOnMainThread_ ema = do
  ch <- getMainThreadChan
  performEvent_ $ ffor ema $ \ma -> do
    unlift <- askUnliftIO
    liftIO $ writeChan ch (unliftIO unlift ma)

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnMainThreadA :: MonadHasMain m => IO a -> m (Async a)
runOnMainThreadA ma = do
  ch <- getMainThreadChan
  liftIO $ do
    resVar <- newEmptyMVar
    writeChan ch $ putMVar resVar =<< ma
    async $ takeMVar resVar

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnMainThreadM :: MonadHasMain m => IO () -> m ()
runOnMainThreadM ma = do
  ch <- getMainThreadChan
  liftIO $ writeChan ch ma
