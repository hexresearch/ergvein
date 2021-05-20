{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Status.Env(
    StatusEnv(..)
  , HasStatusEnv(..)
  , newStatusEnv
  , runStatusEnv
  ) where

import Control.Monad.Reader
import Ergvein.Core.Status.Monad
import Ergvein.Core.Status.Types
import Data.Map (Map)
import Ergvein.Types
import Reflex
import Reflex.ExternalRef

newtype StatusEnv t = StatusEnv {
  stenv'walletStatus  :: ExternalRef t (Map Currency WalletStatus)
}

type StatusM t m = ReaderT (StatusEnv t) m

class Monad m => HasStatusEnv t m | m -> t where
  getStatusEnv :: m (StatusEnv t)

instance Monad m => HasStatusEnv t (StatusM t m) where
  getStatusEnv = ask
  {-# INLINE getStatusEnv #-}

instance (HasStatusEnv t m, MonadStatusConstr t m) => MonadStatus t m where
  getWalletStatusRef = fmap stenv'walletStatus getStatusEnv
  {-# INLINE getWalletStatusRef #-}

newStatusEnv :: (MonadIO m, TriggerEvent t m) => m (StatusEnv t)
newStatusEnv = StatusEnv
  <$> newExternalRef mempty

runStatusEnv :: StatusEnv t -> StatusM t m a -> m a
runStatusEnv = flip runReaderT
