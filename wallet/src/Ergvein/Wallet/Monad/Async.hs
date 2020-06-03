module Ergvein.Wallet.Monad.Async(
    bindSelf
  , forkOnOther
  , performFork
  , performFork_
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Random.Strict
import Data.Text (pack)
import Data.Time
import Ergvein.Text
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Native
import Foreign.JavaScript.TH (WithJSContextSingleton(..))
import Reflex.ExternalRef
import Reflex.Spider.Internal (SpiderHostFrame(..), EventM(..))
import Reflex
import Control.Monad

import qualified Reflex.Profiled as RP

-- | Helper to bind main thread to capability. Simply calls `forkOn` and waits
-- for finish.
bindSelf :: IO () -> IO ()
bindSelf io = do
  var <- newEmptyMVar
  n <- getNumCapabilities
  if n == 1 then io else do
    tid <- forkOn 0 $ io `finally` putMVar var ()
    takeMVar var

-- | Fork new thread and bind it to other capability than current thread.
forkOnOther :: IO () -> IO ThreadId
forkOnOther m = do
  (cap, _) <- threadCapability =<< myThreadId
  n <- getNumCapabilities
  if n == 1 then forkIO m else do
    i <- uniform [i | i <- [0 .. n-1], i /= cap]
    putStrLn $ "Our cap is " ++ show cap ++ ", forking on " ++ show i
    forkOn i m

-- | Helper that runs action in event in new thread with respect for logging of errors.
performFork :: forall t m a . (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), PlatformNatives) => Event t (Performable m a) -> m (Event t a)
performFork em = performEventAsync $ ffor em $ \ma fire -> do
  unlift <- askUnliftIO
  void . liftIO . forkOnOther $ do
    ea :: Either SomeException a <- try $ unliftIO unlift ma
    either (logWrite . ("Forked event failed: " <>) . showt) fire ea

-- | Helper that runs action in event in new thread with respect for logging of errors.
performFork_ :: forall t m . (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), PlatformNatives) => Event t (Performable m ()) -> m ()
performFork_ em = performEvent_ $ ffor em $ \ma -> do
  unlift <- askUnliftIO
  void . liftIO . forkOnOther $ do
    ea :: Either SomeException () <- try $ unliftIO unlift ma
    either (logWrite . ("Forked event failed: " <>) . showt) pure ea

instance MonadUnliftIO m => MonadUnliftIO (WithJSContextSingleton x m) where
  withRunInIO = wrappedWithRunInIO WithJSContextSingleton unWithJSContextSingleton
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO m => MonadUnliftIO (RP.ProfiledM m) where
  withRunInIO = wrappedWithRunInIO ProfiledM runProfiledM
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO (SpiderHostFrame x) where
  withRunInIO = wrappedWithRunInIO SpiderHostFrame runSpiderHostFrame
  {-# INLINE withRunInIO #-}

instance MonadUnliftIO (EventM x) where
  withRunInIO = wrappedWithRunInIO EventM unEventM
  {-# INLINE withRunInIO #-}
