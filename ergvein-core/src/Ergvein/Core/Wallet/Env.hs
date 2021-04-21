module Ergvein.Core.Wallet.Env(
    PreWalletEnv(..)
  , PreWalletM
  , HasPreWalletEnv(..)
  , newPreWalletEnv
  , runPreWallet
  , WalletEnv(..)
  , WalletM
  , HasWalletEnv(..)
  , newWalletEnv
  , runWallet
  , liftWallet
  ) where

import Control.Monad.Reader
import Data.Fixed
import Data.Map (Map)
import Ergvein.Core.Store.Util
import Ergvein.Core.Wallet.Monad
import Ergvein.Types
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Network
import Sepulcas.Native

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data PreWalletEnv t = PreWalletEnv {
  pre'authRef         :: !(ExternalRef t (Maybe WalletInfo))
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
  setWalletInfo e = do
    authRef <- fmap pre'authRef getPreWalletEnv
    performEvent $ ffor e $ \v -> do
      logWrite "unauthed setWalletInfo: setting info"
      lift $ setLastStorage $ _storage'walletName . _walletInfo'storage <$> v
      writeExternalRef authRef v
  {-# INLINE setWalletInfo #-}

newPreWalletEnv :: (MonadIO m, TriggerEvent t m) => m (PreWalletEnv t)
newPreWalletEnv = PreWalletEnv
  <$> newExternalRef Nothing

runPreWallet :: PreWalletEnv t -> PreWalletM t m a -> m a
runPreWallet = flip runReaderT

data WalletEnv t = WalletEnv {
  env'authRef         :: !(ExternalRef t WalletInfo)
, env'logout          :: !(EventTrigger t ())
, env'filtersSyncRef  :: !(ExternalRef t (Map Currency Bool))
, env'activeCursRef   :: !(ExternalRef t (S.Set Currency))
, env'feesStore       :: !(ExternalRef t (Map Currency FeeBundle))
, env'ratesRef        :: !(ExternalRef t (Map Currency (Map Fiat Centi)))
}

type WalletM t m = ReaderT (WalletEnv t) m

class HasWalletEnv t m | m -> t where
  getWalletEnv :: m (WalletEnv t)

instance Monad m => HasWalletEnv t (WalletM t m) where
  getWalletEnv = ask
  {-# INLINE getWalletEnv #-}

instance {-# OVERLAPPING #-} (MonadPreWalletConstr t m, HasStoreDir (Performable m)) => MonadPreWallet t (WalletM t m) where
  getWalletInfoMaybeRef = fmapExternalRef Just =<< fmap env'authRef getWalletEnv
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = do
    WalletEnv{..} <- getWalletEnv
    performEvent $ ffor e $ \case
      Nothing -> do
        logWrite "authed setWalletInfo: logout"
        lift $ setLastStorage Nothing
        liftIO $ triggerFire env'logout ()
      Just v -> do
        logWrite "authed setWalletInfo: changing auth info"
        lift $ setLastStorage $ Just . _storage'walletName . _walletInfo'storage $ v
        writeExternalRef env'authRef v
  {-# INLINE setWalletInfo #-}

instance {-# OVERLAPPABLE #-} (HasWalletEnv t m, MonadWalletConstr t m) => MonadWallet t m where
  getFiltersSyncRef = fmap env'filtersSyncRef getWalletEnv
  {-# INLINE getFiltersSyncRef #-}
  getActiveCursRef = fmap env'activeCursRef getWalletEnv
  {-# INLINE getActiveCursRef #-}
  getWalletInfoRef = fmap env'authRef getWalletEnv
  {-# INLINE getWalletInfoRef #-}
  getFeesRef = fmap env'feesStore getWalletEnv
  {-# INLINE getFeesRef #-}
  getRatesRef = fmap env'ratesRef getWalletEnv
  {-# INLINE getRatesRef #-}

newWalletEnv :: (MonadIO m, TriggerEvent t m) => WalletInfo -> EventTrigger t () -> m (WalletEnv t)
newWalletEnv winfo logoutTrigger = do
  WalletEnv
    <$> newExternalRef winfo
    <*> pure logoutTrigger
    <*> newExternalRef mempty
    <*> newExternalRef mempty
    <*> newExternalRef mempty
    <*> newExternalRef mempty

runWallet :: WalletEnv t -> WalletM t m a -> m a
runWallet = flip runReaderT

-- | Execute action under authorized context or return the given value as result
-- if user is not authorized. Each time the login info changes and walletInfo'isUpdate flag is set to 'False'
-- (user logs out or logs in) the widget is updated.
liftWallet :: forall t m a . MonadPreWallet t m => m a -> WalletM t m a -> m (Dynamic t a)
liftWallet ma0 ma = do
  mauthD <- getWalletInfoMaybe
  mauth0 <- sample . current $ mauthD
  logoutTrigger <- newTriggerEvent'
  let
    run :: WalletInfo -> m a
    run winfo = do
      e <- newWalletEnv winfo logoutTrigger
      runWallet e ma
    ma0' = maybe ma0 run mauth0
    newWalletInfoE = ffilter isMauthUpdate $ updated mauthD
    redrawE = leftmost [newWalletInfoE, Nothing <$ triggerEvent logoutTrigger]
  networkHold ma0' $ ffor redrawE $ maybe ma0 run

isMauthUpdate :: Maybe WalletInfo -> Bool
isMauthUpdate mauth = case mauth of
  Nothing -> True
  Just auth -> not $ _walletInfo'isUpdate auth
