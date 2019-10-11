{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Env(
    Env(..)
  , newEnv
  , runEnv
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor (void)
import Data.Text (Text)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable

data Env = Env {
  env'settings  :: !Settings
}

newEnv :: MonadIO m => Settings -> m Env
newEnv settings = liftIO $ do
  pure Env {
      env'settings = settings
    }

instance MonadFrontConstr t m => MonadFront t (ReaderT Env m) where
  getSettings = asks env'settings
  {-# INLINE getSettings #-}

runEnv :: TriggerEvent t m => Env -> ReaderT Env (RetractT t m) a -> m a
runEnv e ma = do
  re <- newRetractEnv
  runRetractT (runReaderT ma e) re
