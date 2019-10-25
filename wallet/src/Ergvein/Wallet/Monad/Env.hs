{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Env(
    Env(..)
  , newEnv
  , runEnv
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor (void)
import Data.IORef
import Data.Text (Text)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Settings
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable

data Env t = Env {
  env'settings  :: !Settings
, env'backEvent :: !(Event t ())
, env'backFire  :: !(IO ())
, env'loading   :: !(Event t (Text, Bool), (Text, Bool) -> IO ())
}

newEnv :: (Reflex t, TriggerEvent t m) => Settings -> m (Env t)
newEnv settings = do
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  pure Env {
      env'settings  = settings
    , env'backEvent = backE
    , env'backFire  = backFire ()
    , env'loading   = loadingEF
    }

instance MonadFrontConstr t m => MonadFront t (ReaderT (Env t) m) where
  getSettings = asks env'settings
  {-# INLINE getSettings #-}
  getBackEvent = asks env'backEvent
  {-# INLINE getBackEvent #-}
  getLoadingWidgetTF = asks env'loading
  {-# INLINE getLoadingWidgetTF #-}
  toggleLoadingWidget reqE = do
    fire <- asks (snd . env'loading)
    performEvent_ $ (liftIO . fire) <$> reqE
  {-# INLINE toggleLoadingWidget #-}
  loadingWidgetDyn reqD = do
    fire <- asks (snd . env'loading)
    performEvent_ $ (liftIO . fire) <$> (updated reqD)
  {-# INLINE loadingWidgetDyn #-}

runEnv :: MonadBaseConstr t m
  => RunCallbacks -> Env t -> ReaderT (Env t) (RetractT t m) a -> m a
runEnv cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ env'backFire e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = systemBackButton >> ma

systemBackButton :: MonadFront t m => m ()
systemBackButton = void $ retract =<< getBackEvent
