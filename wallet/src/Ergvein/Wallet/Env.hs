{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Env(
    MonadFront(..)
  , MonadFrontConstr
  , Env(..)
  , newEnv
  , runEnv
  -- * Reexports
  , Text
  , void
  , module Reflex.Dom
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor (void)
import Data.Text (Text)
import Ergvein.Wallet.Settings
import Reflex
import Reflex.Dom

type MonadFrontConstr t m = (MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , PerformEvent t m
  , MonadIO (Performable m)
  , MonadSample t (Performable m)
  , MonadIO m
  , TriggerEvent t m
  , DomBuilderSpace m ~ GhcjsDomSpace)

class MonadFrontConstr t m => MonadFront t m | m -> t where
  getSettings :: m Settings

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
