module Ergvein.Wallet.Monad.Unauth
  (
    UnauthEnv(..)
  , newEnv
  , runEnv
  ) where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Crypto.Random.Types
import Data.IORef
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.Socket (SockAddr)
import Reflex.Dom.Retractable
import Reflex.ExternalRef

import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Ergvein.Types.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Sepulcas.Native
import Sepulcas.Monad
import Sepulcas.Run.Callbacks
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Version
import Ergvein.Wallet.Worker.Indexer
import Ergvein.Wallet.Worker.NodeDiscovery

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data UnauthEnv t = UnauthEnv {
  unauth'settings        :: !(ExternalRef t Settings)
, unauth'sepulca         :: !(Sepulca t)
, unauth'authRef         :: !(ExternalRef t (Maybe AuthInfo))
-- Client context
, unauth'addrsArchive    :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, unauth'inactiveAddrs   :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, unauth'activeAddrs     :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, unauth'indexConmap     :: !(ExternalRef t (Map ErgveinNodeAddr (IndexerConnection t)))
, unauth'indexStatus     :: !(ExternalRef t (Map ErgveinNodeAddr IndexerStatus))
, unauth'reqUrlNum       :: !(ExternalRef t (Int, Int))
, unauth'actUrlNum       :: !(ExternalRef t Int)
, unauth'timeout         :: !(ExternalRef t NominalDiffTime)
, unauth'indexReqSel     :: !(IndexReqSelector t)
, unauth'indexReqFire    :: !(Map ErgveinNodeAddr IndexerMsg -> IO ())
, unauth'activateIndexEF :: !(Event t [ErgveinNodeAddr], [ErgveinNodeAddr] -> IO ())
}

type UnauthM t m = ReaderT (UnauthEnv t) m

instance Monad m => HasSepulca t (UnauthM t m) where
  getSepulca = asks unauth'sepulca
  {-# INLINE getSepulca #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives, HasVersion) => MonadFrontBase t (UnauthM t m) where
  getAuthInfoMaybeRef = asks unauth'authRef
  {-# INLINE getAuthInfoMaybeRef #-}
  setAuthInfo e = do
    authRef <- asks unauth'authRef
    performEvent $ ffor e $ \v -> do
      logWrite "unauthed setAuthInfo: setting info"
      setLastStorage $ _storage'walletName . _authInfo'storage <$> v
      writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}

instance MonadBaseConstr t m => MonadHasSettings t (UnauthM t m) where
  getSettingsRef = asks unauth'settings
  {-# INLINE getSettingsRef #-}

instance MonadBaseConstr t m => MonadAlertPoster t (UnauthM t m) where
  postAlert e = do
    (_, fire) <- asks (sepulca'alertsEF . unauth'sepulca)
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . sepulca'alertsEF . unauth'sepulca)
  getAlertEventFire = asks (sepulca'alertsEF . unauth'sepulca)
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance MonadBaseConstr t m => MonadIndexClient t (UnauthM t m) where
  getActiveAddrsRef = asks unauth'activeAddrs
  {-# INLINE getActiveAddrsRef #-}
  getArchivedAddrsRef = asks unauth'addrsArchive
  {-# INLINE getArchivedAddrsRef #-}
  getActiveConnsRef = asks unauth'indexConmap
  {-# INLINE getActiveConnsRef #-}
  getStatusConnsRef = asks unauth'indexStatus
  {-# INLINE getStatusConnsRef #-}
  getInactiveAddrsRef = asks unauth'inactiveAddrs
  {-# INLINE getInactiveAddrsRef #-}
  getActiveUrlsNumRef = asks unauth'actUrlNum
  {-# INLINE getActiveUrlsNumRef #-}
  getRequiredUrlNumRef = asks unauth'reqUrlNum
  {-# INLINE getRequiredUrlNumRef #-}
  getRequestTimeoutRef = asks unauth'timeout
  {-# INLINE getRequestTimeoutRef #-}
  getIndexReqSelector = asks unauth'indexReqSel
  {-# INLINE getIndexReqSelector #-}
  getIndexReqFire = asks unauth'indexReqFire
  {-# INLINE getIndexReqFire #-}
  getActivationEF = asks unauth'activateIndexEF
  {-# INLINE getActivationEF #-}

newEnv :: MonadBaseConstr t m
  => Settings
  -> Chan (IO ()) -- UI callbacks channel
  -> m (UnauthEnv t)
newEnv settings uiChan = do
  settingsRef <- newExternalRef settings
  sepulca <- newSepulca (Just $ settingsStoreDir settings) (settingsLang settings) uiChan
  authRef <- newExternalRef Nothing

  -- MonadClient refs
  rs <- runReaderT mkResolvSeed (uiChan, settingsRef)

  socadrs         <- fmap namedAddrName <$> resolveAddrs rs defIndexerPort (settingsActiveAddrs settings)
  urlsArchive     <- newExternalRef . S.fromList . fmap namedAddrName =<< resolveAddrs rs defIndexerPort (settingsArchivedAddrs settings)
  inactiveUrls    <- newExternalRef . S.fromList . fmap namedAddrName =<< resolveAddrs rs defIndexerPort (settingsDeactivatedAddrs settings)
  actvieAddrsRef  <- newExternalRef $ S.fromList socadrs
  indexConmapRef  <- newExternalRef $ M.empty
  indexStatusRef  <- newExternalRef $ M.empty
  reqUrlNumRef    <- newExternalRef $ settingsReqUrlNum settings
  actUrlNumRef    <- newExternalRef $ settingsActUrlNum settings
  timeoutRef      <- newExternalRef $ settingsReqTimeout settings
  (iReqE, iReqFire) <- newTriggerEvent
  let indexSel = fanMap iReqE -- Node request selector :: NodeReqSelector t
  indexEF <- newTriggerEvent
  let env = UnauthEnv {
          unauth'settings         = settingsRef
        , unauth'sepulca          = sepulca
        , unauth'authRef          = authRef
        , unauth'addrsArchive     = urlsArchive
        , unauth'inactiveAddrs    = inactiveUrls
        , unauth'activeAddrs      = actvieAddrsRef
        , unauth'indexConmap      = indexConmapRef
        , unauth'indexStatus      = indexStatusRef
        , unauth'reqUrlNum        = reqUrlNumRef
        , unauth'actUrlNum        = actUrlNumRef
        , unauth'timeout          = timeoutRef
        , unauth'indexReqSel      = indexSel
        , unauth'indexReqFire     = iReqFire
        , unauth'activateIndexEF  = indexEF
        }
  flip runReaderT env $ do
    ensureErgveinNetwork
    indexerNodeController socadrs
  pure env

runEnv :: (MonadBaseConstr t m, PlatformNatives, HasVersion)
  => RunCallbacks -> UnauthEnv t -> UnauthM t (SepulcaM t (RetractT t m)) a -> m a
runEnv cbs e ma = runSepulca cbs (unauth'sepulca e) (runReaderT ma e)
