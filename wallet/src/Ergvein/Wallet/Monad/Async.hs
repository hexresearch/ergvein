module Ergvein.Wallet.Monad.Async(
    performFork
  , performFork_
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
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

-- | Helper that runs action in event in new thread with respect for logging of errors.
performFork :: forall t m a . (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), PlatformNatives) => Event t (Performable m a) -> m (Event t a)
performFork em = performEventAsync $ ffor em $ \ma fire -> do
  unlift <- askUnliftIO
  void . liftIO . forkIO $ do
    ea :: Either SomeException a <- try $ unliftIO unlift ma
    either (logWrite . ("Forked event failed: " <>) . showt) fire ea

-- | Helper that runs action in event in new thread with respect for logging of errors.
performFork_ :: forall t m . (PerformEvent t m, TriggerEvent t m, MonadUnliftIO (Performable m), PlatformNatives) => Event t (Performable m ()) -> m ()
performFork_ em = performEvent_ $ ffor em $ \ma -> do
  unlift <- askUnliftIO
  void . liftIO . forkIO $ do
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
