{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Settings.Env(
    SettingsEnv(..)
  , SettingsM
  , HasSettingsEnv(..)
  , newSettingsEnv
  , runSettings
  ) where

import Control.Monad.Reader
import Ergvein.Core.Settings.Monad
import Reflex
import Reflex.ExternalRef

newtype SettingsEnv t = SettingsEnv {
  envSettings :: ExternalRef t Settings
}

type SettingsM t m = ReaderT (SettingsEnv t) m

class Monad m => HasSettingsEnv t m | m -> t where
  getSettingsEnv :: m (SettingsEnv t)

instance Monad m => HasSettingsEnv t (SettingsM t m) where
  getSettingsEnv = ask
  {-# INLINE getSettingsEnv #-}

instance (HasSettingsEnv t m, MonadSettingsConstr t m) => MonadSettings t m where
  getSettingsRef = fmap envSettings getSettingsEnv
  {-# INLINE getSettingsRef #-}

newSettingsEnv :: (MonadIO m, TriggerEvent t m) => Settings -> m (SettingsEnv t)
newSettingsEnv setts = SettingsEnv
  <$> newExternalRef setts

runSettings :: SettingsEnv t -> SettingsM t m a -> m a
runSettings = flip runReaderT
