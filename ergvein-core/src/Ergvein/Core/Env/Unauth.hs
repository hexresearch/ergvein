module Ergvein.Core.Env.Unauth(
    UnauthEnv(..)
  , HasUnauthEnv(..)
  , UnauthM
  , newUnauthEnv
  , runUnauth
  ) where

import Control.Monad.Reader
import Data.Proxy
import Ergvein.Core.Client
import Ergvein.Core.Password
import Ergvein.Core.Settings
import Ergvein.Core.Wallet
import Reflex.Main.Thread
import Sepulcas.Native

data UnauthEnv t = UnauthEnv {
  unauth'settings :: !(SettingsEnv t)
, unauth'wallet   :: !(PreWalletEnv t)
, unauth'client   :: !(ClientEnv t)
, unauth'pass     :: !(PassEnv t)
}

type UnauthM t m = ReaderT (UnauthEnv t) m

class Monad m => HasUnauthEnv t m | m -> t where
  getUnauthEnv :: m (UnauthEnv t)

instance Monad m => HasUnauthEnv t (UnauthM t m) where
  getUnauthEnv = ask
  {-# INLINE getUnauthEnv #-}

instance Monad m => HasSettingsEnv t (UnauthM t m) where
  getSettingsEnv = asks unauth'settings
  {-# INLINE getSettingsEnv #-}

instance Monad m => HasPreWalletEnv t (UnauthM t m) where
  getPreWalletEnv = asks unauth'wallet
  {-# INLINE getPreWalletEnv #-}

instance Monad m => HasClientEnv t (UnauthM t m) where
  getClientEnv = asks unauth'client
  {-# INLINE getClientEnv #-}

instance Monad m => HasPassEnv t (UnauthM t m) where
  getPassEnv = asks unauth'pass
  {-# INLINE getPassEnv #-}

instance {-# OVERLAPPABLE #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (UnauthM t m) where
  getWalletInfoMaybeRef = runReaderT getWalletInfoMaybeRef =<< asks unauth'wallet
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = runReaderT (setWalletInfo e) =<< asks unauth'wallet
  {-# INLINE setWalletInfo #-}

  setWalletInfoNow _ v = lift . runReaderT (setWalletInfoNow (Proxy :: Proxy (PreWalletM t m)) v) =<< asks unauth'wallet
  {-# INLINE setWalletInfoNow #-}

newUnauthEnv :: (MonadIO m, TriggerEvent t m, MonadHasMain m, MonadSettingsConstr t m) => Settings -> m (UnauthEnv t)
newUnauthEnv settings = do
  senv <- newSettingsEnv settings
  UnauthEnv
    <$> pure senv
    <*> newPreWalletEnv
    <*> runReaderT newClientEnv senv
    <*> newPassEnv

runUnauth :: UnauthEnv t -> UnauthM t m a -> m a
runUnauth = flip runReaderT
