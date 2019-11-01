module Ergvein.Wallet.Monad.Util
  (
    runOnUiThread
  , runOnUiThread_
  , runOnUiThreadA
  , runOnUiThreadM
  ) where

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Ergvein.Wallet.Monad.Base
import Reflex

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread :: (MonadBaseConstr t m, HasUIThread m) => Event t (IO a) -> m (Event t a)
runOnUiThread ema = do
  ch <- getUiChan
  performEventAsync $ ffor ema $ \ma fire -> liftIO $ writeChan ch $ fire =<< ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread_ :: (MonadBaseConstr t m, HasUIThread m) => Event t (IO ()) -> m ()
runOnUiThread_ ema = do
  ch <- getUiChan
  performEvent_ $ ffor ema $ \ma -> liftIO $ writeChan ch ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadA :: (MonadBaseConstr t m, HasUIThread m) => IO a -> m (Async a)
runOnUiThreadA ma = do
  ch <- getUiChan
  liftIO $ do
    resVar <- newEmptyMVar
    writeChan ch $ putMVar resVar =<< ma
    async $ takeMVar resVar

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadM :: (MonadBaseConstr t m, HasUIThread m) => IO () -> m ()
runOnUiThreadM ma = do
  ch <- getUiChan
  liftIO $ writeChan ch ma
