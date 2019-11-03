module Ergvein.Wallet.Monad.Util
  (
    runOnUiThread
  , runOnUiThread_
  , runOnUiThreadA
  , runOnUiThreadM
  , nameSpace
  ) where

import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.Async
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Reflex
import Reflex.ExternalRef

-- | Wrap log name space for given widget
nameSpace :: MonadFrontBase t m => Text -> m a -> m a
nameSpace n ma = do
  ref <- getLogsNameSpacesRef
  ns <- modifyExternalRef ref $ \ns -> (n:ns, ns)
  a <- ma
  writeExternalRef ref ns
  pure a

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread :: MonadFrontBase t m => Event t (IO a) -> m (Event t a)
runOnUiThread ema = do
  ch <- getUiChan
  performEventAsync $ ffor ema $ \ma fire -> liftIO $ writeChan ch $ fire =<< ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThread_ :: MonadFrontBase t m => Event t (IO ()) -> m ()
runOnUiThread_ ema = do
  ch <- getUiChan
  performEvent_ $ ffor ema $ \ma -> liftIO $ writeChan ch ma

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadA :: MonadFrontBase t m => IO a -> m (Async a)
runOnUiThreadA ma = do
  ch <- getUiChan
  liftIO $ do
    resVar <- newEmptyMVar
    writeChan ch $ putMVar resVar =<< ma
    async $ takeMVar resVar

-- | Execute the action in main thread of UI. Very useful for android API actions
-- that must be executed in the same thread where Looper was created.
runOnUiThreadM :: MonadFrontBase t m => IO () -> m ()
runOnUiThreadM ma = do
  ch <- getUiChan
  liftIO $ writeChan ch ma
