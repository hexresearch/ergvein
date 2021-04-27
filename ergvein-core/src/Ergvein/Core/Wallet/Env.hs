{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Wallet.Env(
    PreWalletEnv(..)
  , PreWalletM
  , HasPreWalletEnv(..)
  , newPreWalletEnv
  , runPreWallet
  , WalletEnv(..)
  , toPreWalletEnv
  , WalletM
  , HasWalletEnv(..)
  , newWalletEnv
  , runWallet
  , LiftWallet(..)
  , liftWallet
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Data.Fixed
import Data.Map (Map)
import Data.Proxy
import Ergvein.Core.Store.Util
import Ergvein.Core.Wallet.Monad
import Ergvein.Types
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Network
import Sepulcas.Native

import qualified Data.Set as S

data PreWalletEnv t = PreWalletEnv {
  pre'authRef         :: !(ExternalRef t (Maybe WalletInfo))
, pre'authMutex       :: !(MVar ())
}

type PreWalletM t m = ReaderT (PreWalletEnv t) m

class Monad m => HasPreWalletEnv t m | m -> t where
  getPreWalletEnv :: m (PreWalletEnv t)

instance Monad m => HasPreWalletEnv t (PreWalletM t m) where
  getPreWalletEnv = ask
  {-# INLINE getPreWalletEnv #-}

instance {-# OVERLAPPABLE #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (PreWalletM t m) where
  getWalletInfoMaybeRef = fmap pre'authRef getPreWalletEnv
  {-# INLINE getWalletInfoMaybeRef #-}
  setWalletInfo :: Event t (Maybe WalletInfo) -> PreWalletM t m (Event t ())
  setWalletInfo e = performEvent $ setWalletInfoNow (Proxy :: Proxy (PreWalletM t m)) <$> e
  {-# INLINE setWalletInfo #-}
  setWalletInfoNow _ v = do
    logWrite "unauthed setWalletInfo: setting info"
    authRef <- asks pre'authRef
    lift $ setLastStorage $ _storage'walletName . _walletInfo'storage <$> v
    writeExternalRef authRef v
  {-# INLINE setWalletInfoNow #-}
  getWalletInfoMutex = fmap pre'authMutex getPreWalletEnv
  {-# INLINE getWalletInfoMutex #-}

newPreWalletEnv :: (MonadIO m, TriggerEvent t m) => m (PreWalletEnv t)
newPreWalletEnv = PreWalletEnv
  <$> newExternalRef Nothing
  <*> liftIO (newMVar ())

runPreWallet :: PreWalletEnv t -> PreWalletM t m a -> m a
runPreWallet = flip runReaderT

data WalletEnv t = WalletEnv {
  env'authRef         :: !(ExternalRef t (Maybe WalletInfo))
, env'authMutex       :: !(MVar ())
, env'logout          :: !(EventTrigger t ())
, env'filtersSyncRef  :: !(ExternalRef t (Map Currency Bool))
, env'activeCursRef   :: !(ExternalRef t (S.Set Currency))
, env'feesStore       :: !(ExternalRef t (Map Currency FeeBundle))
, env'ratesRef        :: !(ExternalRef t (Map Currency (Map Fiat Centi)))
}

toPreWalletEnv :: WalletEnv t -> PreWalletEnv t
toPreWalletEnv WalletEnv{..} = PreWalletEnv env'authRef env'authMutex

type WalletM t m = ReaderT (WalletEnv t) m

class HasWalletEnv t m | m -> t where
  getWalletEnv :: m (WalletEnv t)

instance Monad m => HasWalletEnv t (WalletM t m) where
  getWalletEnv = ask
  {-# INLINE getWalletEnv #-}

instance {-# OVERLAPPING #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (WalletM t m) where
  getWalletInfoMaybeRef = fmap env'authRef getWalletEnv
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo :: Event t (Maybe WalletInfo) -> WalletM t m (Event t ())
  setWalletInfo e = performEvent $ setWalletInfoNow (Proxy :: Proxy (WalletM t m)) <$> e
  {-# INLINE setWalletInfo #-}

  setWalletInfoNow _ = \case
    Nothing -> do
      logWrite "authed setWalletInfo: logout"
      WalletEnv{..} <- ask
      lift $ setLastStorage Nothing
      liftIO $ triggerFire env'logout ()
    Just v -> do
      logWrite "authed setWalletInfo: changing auth info"
      WalletEnv{..} <- ask
      lift $ setLastStorage $ Just . _storage'walletName . _walletInfo'storage $ v
      writeExternalRef env'authRef (Just v)
  {-# INLINE setWalletInfoNow #-}

  getWalletInfoMutex = fmap env'authMutex getWalletEnv
  {-# INLINE getWalletInfoMutex #-}

instance {-# OVERLAPPABLE #-} (HasWalletEnv t m, MonadWalletConstr t m) => MonadWallet t m where
  getFiltersSyncRef = fmap env'filtersSyncRef getWalletEnv
  {-# INLINE getFiltersSyncRef #-}
  getActiveCursRef = fmap env'activeCursRef getWalletEnv
  {-# INLINE getActiveCursRef #-}
  getWalletInfo = do
    minfo <- externalRefDynamic =<< fmap env'authRef getWalletEnv
    pure $ maybe (error "Impossible: Nothing in wallet info in MonadWallet!") id <$> minfo
  {-# INLINE getWalletInfo #-}
  getFeesRef = fmap env'feesStore getWalletEnv
  {-# INLINE getFeesRef #-}
  getRatesRef = fmap env'ratesRef getWalletEnv
  {-# INLINE getRatesRef #-}

newWalletEnv :: (MonadIO m, TriggerEvent t m) => WalletInfo -> EventTrigger t () -> m (WalletEnv t)
newWalletEnv winfo logoutTrigger = do
  WalletEnv
    <$> newExternalRef (Just winfo)
    <*> liftIO (newMVar ())
    <*> pure logoutTrigger
    <*> newExternalRef mempty
    <*> newExternalRef mempty
    <*> newExternalRef mempty
    <*> newExternalRef mempty

runWallet :: WalletEnv t -> WalletM t m a -> m a
runWallet = flip runReaderT

class LiftWallet t n m | m -> t where
  -- | Way to execute authed monad with given wallet info and logout trigger
  hoistWallet :: WalletInfo -> EventTrigger t () -> n a -> m a

-- | Execute action under authorized context or return the given value as result
-- if user is not authorized. Each time the login info changes and walletInfo'isUpdate flag is set to 'False'
-- (user logs out or logs in) the widget is updated.
liftWallet :: forall t m n a . (MonadPreWallet t m, LiftWallet t n m)
  => m a -> n a -> m (Dynamic t a)
liftWallet ma0 ma = do
  mauthD <- getWalletInfoMaybe
  mauth0 <- sample . current $ mauthD
  logoutTrigger <- newTriggerEvent'
  let
    run :: WalletInfo -> m a
    run winfo = hoistWallet winfo logoutTrigger ma
    ma0' = maybe ma0 run mauth0
    newWalletInfoE = ffilter isMauthUpdate $ updated mauthD
    redrawE = leftmost [newWalletInfoE, Nothing <$ triggerEvent logoutTrigger]
  networkHold ma0' $ ffor redrawE $ maybe ma0 run

isMauthUpdate :: Maybe WalletInfo -> Bool
isMauthUpdate mauth = case mauth of
  Nothing -> True
  Just auth -> not $ _walletInfo'isUpdate auth
