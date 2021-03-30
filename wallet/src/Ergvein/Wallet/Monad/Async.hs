{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Monad.Async(
    bindSelf
  , forkOnOther
  , performFork
  , performFork_
  ) where

import Control.Concurrent
import Control.Concurrent.Async hiding (ExceptionInLinkedThread(..))
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Random.Strict
import Ergvein.Text
import Ergvein.Wallet.Native
import Ergvein.Concurrent (ExceptionInLinkedThread(..), defaultIgnoredExceptions)
import Foreign.JavaScript.TH (WithJSContextSingleton(..))
import Reflex.Spider.Internal (SpiderHostFrame(..), EventM(..))
import Reflex

import qualified Reflex.Profiled as RP

-- | Helper to bind main thread to capability. Simply calls `forkOn` and waits
-- for finish.
bindSelf :: IO () -> IO ()
bindSelf io = do
  var <- newEmptyMVar
  n <- getNumCapabilities
  if n == 1 then io else do
    void $ forkOn 0 $ io `finally` putMVar var ()
    takeMVar var

-- | Fork new thread and bind it to other capability than current thread.
forkOnOther :: IO () -> IO ThreadId
forkOnOther m = capabilityToForkOn >>= \case
  Nothing -> forkIO m
  Just i  -> forkOn i m

withLinkedWorkerOnOther :: (MonadUnliftIO m) => m a -> m b -> m b
withLinkedWorkerOnOther action cont = do
  unlift <- askUnliftIO
  liftIO $ do
    tid     <- myThreadId
    doAsync <- maybe async asyncOn <$> capabilityToForkOn
    mask $ \restore -> do
      -- Here we spawn worker thread which will throw unhandled exception to main thread.
      a <- doAsync $ unliftIO unlift action `catch` \e -> do
        unless (defaultIgnoredExceptions e) $ throwTo tid (ExceptionInLinkedThread e)
        throwIO e
      unliftIO unlift cont `finally` cancel a

capabilityToForkOn :: IO (Maybe Int)
capabilityToForkOn = do
  (cap, _) <- threadCapability =<< myThreadId
  getNumCapabilities >>= \case
    1 -> pure Nothing
    n -> Just <$> uniform [i | i <- [0 .. n-1], i /= cap]


-- | Helper that runs action in event in new thread with respect for logging of errors.
performFork
  :: forall t m a . (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), PlatformNatives)
  => Event t (Performable m a) -> m (Event t a)
performFork em = performEventAsync $ ffor em $ \ma fire -> do
  unlift <- askUnliftIO
  void . liftIO . forkOnOther $ do
    ea :: Either SomeException a <- try $ unliftIO unlift ma
    either (logWrite . ("Forked event failed: " <>) . showt) fire ea

-- | Helper that runs action in event in new thread with respect for logging of errors.
performFork_
  :: forall t m . (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), PlatformNatives)
  => Event t (Performable m ()) -> m ()
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
