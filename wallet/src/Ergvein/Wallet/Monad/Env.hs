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
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Monad
import Reflex
import Reflex.Dom

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

runEnv :: Env -> ReaderT Env m a -> m a
runEnv = flip runReaderT
