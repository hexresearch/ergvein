module Ergvein.Core.Env.Auth(
    AuthEnv(..)
  , HasAuthEnv(..)
  , AuthM
  , newAuthEnv
  , unauthEnv
  , runAuth
  ) where

import Control.Monad.Reader
import Data.Proxy
import Ergvein.Core.Client
import Ergvein.Core.Env.Unauth
import Ergvein.Core.Node
import Ergvein.Core.Password
import Ergvein.Core.Settings
import Ergvein.Core.Status
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Types
import Reflex.Flunky
import Reflex.Main.Thread
import Sepulcas.Native

data AuthEnv t = AuthEnv {
  auth'settings :: !(SettingsEnv t)
, auth'wallet   :: !(WalletEnv t)
, auth'node     :: !(NodeEnv t)
, auth'store    :: !(StoreEnv t)
, auth'client   :: !(ClientEnv t)
, auth'pass     :: !(PassEnv t)
, auth'status   :: !(StatusEnv t)
}

type AuthM t m = ReaderT (AuthEnv t) m

class Monad m => HasAuthEnv t m | m -> t where
  getAuthEnv :: m (AuthEnv t)

instance Monad m => HasAuthEnv t (AuthM t m) where
  getAuthEnv = ask
  {-# INLINE getAuthEnv #-}

instance Monad m => HasSettingsEnv t (AuthM t m) where
  getSettingsEnv = asks auth'settings
  {-# INLINE getSettingsEnv #-}

instance Monad m => HasWalletEnv t (AuthM t m) where
  getWalletEnv = asks auth'wallet
  {-# INLINE getWalletEnv #-}

instance Monad m => HasNodeEnv t (AuthM t m) where
  getNodeEnv = asks auth'node
  {-# INLINE getNodeEnv #-}

instance Monad m => HasStoreEnv t (AuthM t m) where
  getStoreEnv = asks auth'store
  {-# INLINE getStoreEnv #-}

instance Monad m => HasClientEnv t (AuthM t m) where
  getClientEnv = asks auth'client
  {-# INLINE getClientEnv #-}

instance Monad m => HasPassEnv t (AuthM t m) where
  getPassEnv = asks auth'pass
  {-# INLINE getPassEnv #-}

instance Monad m => HasStatusEnv t (AuthM t m) where
  getStatusEnv = asks auth'status
  {-# INLINE getStatusEnv #-}

instance {-# OVERLAPPABLE #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (AuthM t m) where
  getWalletInfoMaybeRef = runReaderT getWalletInfoMaybeRef =<< asks auth'wallet
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = runReaderT (setWalletInfo e) =<< asks auth'wallet
  {-# INLINE setWalletInfo #-}

  setWalletInfoNow _ v = lift . runReaderT (setWalletInfoNow (Proxy :: Proxy (WalletM t m)) v) =<< asks auth'wallet
  {-# INLINE setWalletInfoNow #-}

newAuthEnv :: (MonadHasMain m, MonadSettingsConstr t m) => UnauthEnv t -> WalletInfo -> EventTrigger t () -> m (AuthEnv t)
newAuthEnv UnauthEnv{..} winfo logoutTrigger = do
  wenv <- newWalletEnv winfo logoutTrigger
  AuthEnv
    <$> pure unauth'settings
    <*> pure wenv
    <*> newNodeEnv
    <*> newStoreEnv
    <*> pure unauth'client
    <*> pure unauth'pass
    <*> newStatusEnv

unauthEnv :: AuthEnv t -> UnauthEnv t
unauthEnv AuthEnv{..} = UnauthEnv {
    unauth'settings = auth'settings
  , unauth'wallet = toPreWalletEnv auth'wallet
  , unauth'client = auth'client
  , unauth'pass = auth'pass
  }

runAuth :: AuthEnv t -> AuthM t m a -> m a
runAuth = flip runReaderT

instance (MonadSettingsConstr t m, MonadHasMain m) => LiftWallet t (AuthM t m) (UnauthM t m) where
  hoistWallet winfo logoutTrigger mx = do
    uenv <- ask
    aenv <- newAuthEnv uenv winfo logoutTrigger
    lift $ runAuth aenv mx
  {-# INLINE hoistWallet #-}
