module Ergvein.Core.Env.Auth(
    AuthEnv(..)
  , AuthM
  , newAuthEnv
  , runAuth
  ) where

import Control.Monad.Reader
import Ergvein.Core.Client
import Ergvein.Core.Env.Unauth
import Ergvein.Core.Node
import Ergvein.Core.Settings
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
}

type AuthM t m = ReaderT (AuthEnv t) m

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

instance {-# OVERLAPPABLE #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (AuthM t m) where
  getWalletInfoMaybeRef = runReaderT getWalletInfoMaybeRef =<< asks auth'wallet
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = runReaderT (setWalletInfo e) =<< asks auth'wallet
  {-# INLINE setWalletInfo #-}

newAuthEnv :: (MonadHasMain m, MonadSettingsConstr t m) => UnauthEnv t -> WalletInfo -> EventTrigger t () -> m (AuthEnv t)
newAuthEnv UnauthEnv{..} winfo logoutTrigger = do
  wenv <- newWalletEnv winfo logoutTrigger
  AuthEnv
    <$> pure unauth'settings
    <*> pure wenv
    <*> newNodeEnv
    <*> newStoreEnv (env'authRef wenv)
    <*> pure unauth'client

runAuth :: AuthEnv t -> AuthM t m a -> m a
runAuth = flip runReaderT

-- | Execute action under authorized context or return the given value as result
-- if user is not authorized. Each time the login info changes and walletInfo'isUpdate flag is set to 'False'
-- (user logs out or logs in) the widget is updated.
liftAuth :: forall t m a . (MonadPreWallet t m, MonadSettingsConstr t m, MonadHasMain m, HasStoreDir (Performable m))
  => UnauthM t m a -> AuthM t m a -> UnauthM t m (Dynamic t a)
liftAuth ma0 ma = do
  uenv <- ask
  let
    hoister :: forall x . WalletInfo -> EventTrigger t () -> AuthM t m x -> UnauthM t m x
    hoister winfo logoutTrigger mx = do
        aenv <- newAuthEnv uenv winfo logoutTrigger
        lift $ runAuth aenv mx
  liftWallet hoister ma0 ma
