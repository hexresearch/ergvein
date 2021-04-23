module Ergvein.Wallet.Monad.Env(
    BaseEnv(..)
  , BaseM
  , HasBaseEnv(..)
  , newBaseEnv
  , runBase
  , Env(..)
  , ErgveinM
  , liftAuth
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.Proxy
import Ergvein.Core
import Ergvein.Types
import Ergvein.Wallet.Language ()
import Ergvein.Wallet.Version
import Reflex.Dom.Retractable
import Reflex.Flunky
import Sepulcas.Monad
import Sepulcas.Run.Callbacks

data BaseEnv t = BaseEnv {
  benv'sepulca :: !(Sepulca t)
, benv'unauth  :: !(UnauthEnv t)
}

type BaseM t m = ReaderT (BaseEnv t) m

class Monad m => HasBaseEnv t m | m -> t where
  getBaseEnv :: m (BaseEnv t)

instance Monad m => HasBaseEnv t (BaseM t m) where
  getBaseEnv = ask
  {-# INLINE getBaseEnv #-}

instance Monad m => HasSepulca t (BaseM t m) where
  getSepulca = asks benv'sepulca
  {-# INLINE getSepulca #-}

instance Monad m => HasUnauthEnv t (BaseM t m) where
  getUnauthEnv = asks benv'unauth
  {-# INLINE getUnauthEnv #-}

instance Monad m => HasSettingsEnv t (BaseM t m) where
  getSettingsEnv = asks (unauth'settings . benv'unauth)
  {-# INLINE getSettingsEnv #-}

instance Monad m => HasPreWalletEnv t (BaseM t m) where
  getPreWalletEnv = asks (unauth'wallet . benv'unauth)
  {-# INLINE getPreWalletEnv #-}

instance Monad m => HasClientEnv t (BaseM t m) where
  getClientEnv = asks (unauth'client . benv'unauth)
  {-# INLINE getClientEnv #-}

instance Monad m => HasPassEnv t (BaseM t m) where
  getPassEnv = asks (unauth'pass . benv'unauth)
  {-# INLINE getPassEnv #-}

instance Monad m => HasStoreDir (BaseM t m) where
  getStoreDir = sepulca'storeDir <$> getSepulca
  {-# INLINE getStoreDir #-}

-- instance MonadIO m => MonadHasMain (BaseM t m) where
--   getMainThreadChan = sepulca'uiChan <$> getSepulca
--   {-# INLINE getMainThreadChan #-}

instance (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (BaseM t m) where
  getWalletInfoMaybeRef = runReaderT getWalletInfoMaybeRef =<< getPreWalletEnv
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = runReaderT (setWalletInfo e) =<< getPreWalletEnv
  {-# INLINE setWalletInfo #-}

  setWalletInfoNow _ v = lift . runReaderT (setWalletInfoNow (Proxy :: Proxy (PreWalletM t m)) v) =<< getPreWalletEnv
  {-# INLINE setWalletInfoNow #-}

newBaseEnv :: MonadReflex t m
  => Settings
  -> Chan (IO ()) -- ^ UI callbacks channel
  -> m (BaseEnv t)
newBaseEnv settings uiChan = flip runReaderT uiChan $ do
  sepulca <- newSepulca (Just $ settingsStoreDir settings) (settingsLang settings) uiChan
  unauth <- newUnauthEnv settings
  pure $ BaseEnv sepulca unauth

runBase :: (MonadReflex t m, HasVersion) => RunCallbacks -> BaseEnv t -> BaseM t (SepulcaM t (RetractT t m)) a -> m a
runBase cbs e ma = runSepulca cbs (benv'sepulca e) (runReaderT ma e)

data Env t = Env {
  env'sepulca         :: !(Sepulca t)
, env'auth            :: !(AuthEnv t)
}

type ErgveinM t m = ReaderT (Env t) m

newEnv :: (MonadReflex t m, MonadHasMain m) => BaseEnv t -> WalletInfo -> EventTrigger t () -> m (Env t)
newEnv BaseEnv{..} winfo logoutTrigger = do
  auth <- newAuthEnv benv'unauth winfo logoutTrigger
  pure $ Env benv'sepulca auth

instance Monad m => HasSepulca t (ErgveinM t m) where
  getSepulca = asks env'sepulca
  {-# INLINE getSepulca #-}

instance Monad m => HasAuthEnv t (ErgveinM t m) where
  getAuthEnv = asks env'auth
  {-# INLINE getAuthEnv #-}

instance Monad m => HasSettingsEnv t (ErgveinM t m) where
  getSettingsEnv = asks (auth'settings . env'auth)
  {-# INLINE getSettingsEnv #-}

instance Monad m => HasWalletEnv t (ErgveinM t m) where
  getWalletEnv = asks (auth'wallet . env'auth)
  {-# INLINE getWalletEnv #-}

instance Monad m => HasNodeEnv t (ErgveinM t m) where
  getNodeEnv = asks (auth'node . env'auth)
  {-# INLINE getNodeEnv #-}

instance Monad m => HasStoreEnv t (ErgveinM t m) where
  getStoreEnv = asks (auth'store . env'auth)
  {-# INLINE getStoreEnv #-}

instance Monad m => HasClientEnv t (ErgveinM t m) where
  getClientEnv = asks (auth'client . env'auth)
  {-# INLINE getClientEnv #-}

instance Monad m => HasPassEnv t (ErgveinM t m) where
  getPassEnv = asks (auth'pass . env'auth)
  {-# INLINE getPassEnv #-}

instance Monad m => HasStatusEnv t (ErgveinM t m) where
  getStatusEnv = asks (auth'status . env'auth)
  {-# INLINE getStatusEnv #-}

instance Monad m => HasBaseEnv t (ErgveinM t m) where
  getBaseEnv = do
    Env{..} <- ask
    pure $ BaseEnv env'sepulca (unauthEnv env'auth)
  {-# INLINE getBaseEnv #-}

instance {-# OVERLAPPABLE #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (ErgveinM t m) where
  getWalletInfoMaybeRef = runReaderT getWalletInfoMaybeRef =<< getWalletEnv
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = runReaderT (setWalletInfo e) =<< getWalletEnv
  {-# INLINE setWalletInfo #-}

  setWalletInfoNow _ v = lift . runReaderT (setWalletInfoNow (Proxy :: Proxy (WalletM t m)) v) =<< getWalletEnv
  {-# INLINE setWalletInfoNow #-}

instance Monad m => HasPreWalletEnv t (ErgveinM t m) where
  getPreWalletEnv = do
    Env{..} <- ask
    pure $ toPreWalletEnv . auth'wallet $ env'auth
  {-# INLINE getPreWalletEnv #-}

instance (MonadReflex t m, MonadHasMain m) => LiftWallet t (ErgveinM t m) (BaseM t m) where
  hoistWallet winfo logoutTrigger mx = do
    uenv <- ask
    aenv <- newEnv uenv winfo logoutTrigger
    lift $ runReaderT mx aenv
  {-# INLINE hoistWallet #-}

liftAuth :: forall t m a . (MonadReflex t m, MonadPreWallet t m, HasBaseEnv t m, MonadHasMain m, HasStoreDir (Performable m))
  => m a -> ErgveinM t m a -> m (Dynamic t a)
liftAuth ma0 ma = do
  benv <- getBaseEnv
  runReaderT (liftWallet (lift ma0) ma) benv
