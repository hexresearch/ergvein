module Ergvein.Wallet.Monad.Unauth
  (
    UnauthEnv(..)
  , newEnv
  , runEnv
  ) where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.DNS
import Network.Socket (SockAddr)
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Data.Maybe

import Ergvein.Types.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Version
import Ergvein.Wallet.Worker.Indexer

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data UnauthEnv t = UnauthEnv {
  unauth'settings        :: !(ExternalRef t Settings)
, unauth'pauseEF         :: !(Event t (), IO ())
, unauth'resumeEF        :: !(Event t (), IO ())
, unauth'backEF          :: !(Event t (), IO ())
, unauth'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, unauth'langRef         :: !(ExternalRef t Language)
, unauth'storeDir        :: !Text
, unauth'alertsEF        :: !(Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alerts event and trigger
, unauth'logsTrigger     :: !(Event t LogEntry, LogEntry -> IO ())
, unauth'logsNameSpaces  :: !(ExternalRef t [Text])
, unauth'uiChan          :: !(Chan (IO ()))
, unauth'authRef         :: !(ExternalRef t (Maybe AuthInfo))
, unauth'passModalEF     :: !(Event t (Int, Text), (Int, Text) -> IO ())
, unauth'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
-- Client context
, unauth'addrsArchive    :: !(ExternalRef t (S.Set NamedSockAddr))
, unauth'inactiveAddrs   :: !(ExternalRef t (S.Set NamedSockAddr))
, unauth'activeAddrs     :: !(ExternalRef t (S.Set NamedSockAddr))
, unauth'indexConmap     :: !(ExternalRef t (Map SockAddr (IndexerConnection t)))
, unauth'reqUrlNum       :: !(ExternalRef t (Int, Int))
, unauth'actUrlNum       :: !(ExternalRef t Int)
, unauth'timeout         :: !(ExternalRef t NominalDiffTime)
, unauth'indexReqSel     :: !(IndexReqSelector t)
, unauth'indexReqFire    :: !(Map SockAddr IndexerMsg -> IO ())
, unauth'activateIndexEF :: !(Event t [NamedSockAddr], [NamedSockAddr] -> IO ())
}

type UnauthM t m = ReaderT (UnauthEnv t) m

instance Monad m => HasStoreDir (UnauthM t m) where
  getStoreDir = asks unauth'storeDir
  {-# INLINE getStoreDir #-}

instance MonadBaseConstr t m => MonadEgvLogger t (UnauthM t m) where
  getLogsTrigger = asks unauth'logsTrigger
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = asks unauth'logsNameSpaces
  {-# INLINE getLogsNameSpacesRef #-}

instance MonadBaseConstr t m => MonadLocalized t (UnauthM t m) where
  setLanguage lang = do
    langRef <- asks unauth'langRef
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks unauth'langRef
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks unauth'langRef
  {-# INLINE getLanguage #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives, HasVersion) => MonadFrontBase t (UnauthM t m) where
  getLoadingWidgetTF = asks unauth'loading
  {-# INLINE getLoadingWidgetTF #-}
  getPauseEventFire = asks unauth'pauseEF
  {-# INLINE getPauseEventFire #-}
  getResumeEventFire = asks unauth'resumeEF
  {-# INLINE getResumeEventFire #-}
  getBackEventFire = asks unauth'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks unauth'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks unauth'langRef
  {-# INLINE getLangRef #-}
  getAuthInfoMaybeRef = asks unauth'authRef
  {-# INLINE getAuthInfoMaybeRef #-}
  setAuthInfo e = do
    authRef <- asks unauth'authRef
    performEvent $ ffor e $ \v -> do
      logWrite "unauthed setAuthInfo: setting info"
      setLastStorage $ _storage'walletName . _authInfo'storage <$> v
      writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}
  getPasswordModalEF = asks unauth'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks unauth'passSetEF
  {-# INLINE getPasswordSetEF #-}

instance MonadBaseConstr t m => MonadHasSettings t (UnauthM t m) where
  getSettingsRef = asks unauth'settings
  {-# INLINE getSettingsRef #-}

instance MonadBaseConstr t m => MonadAlertPoster t (UnauthM t m) where
  postAlert e = do
    (_, fire) <- asks unauth'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . unauth'alertsEF)
  getAlertEventFire = asks unauth'alertsEF
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
  (pauseE, pauseFire) <- newTriggerEvent
  (resumeE, resumeFire) <- newTriggerEvent
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  alertsEF <- newTriggerEvent
  passSetEF <- newTriggerEvent
  passModalEF <- newTriggerEvent
  authRef <- newExternalRef Nothing
  langRef <- newExternalRef $ settingsLang settings
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []
  -- MonadClient refs
  rs <- liftIO $ resolveSeed $ settingsDns settings

  socadrs         <- parseSockAddrs rs (settingsActiveAddrs settings)
  urlsArchive     <- newExternalRef . S.fromList =<< parseSockAddrs rs (settingsArchivedAddrs settings)
  inactiveUrls    <- newExternalRef . S.fromList =<< parseSockAddrs rs (settingsDeactivatedAddrs settings)
  actvieAddrsRef  <- newExternalRef $ S.fromList socadrs
  indexConmapRef  <- newExternalRef $ M.empty
  reqUrlNumRef    <- newExternalRef $ settingsReqUrlNum settings
  actUrlNumRef    <- newExternalRef $ settingsActUrlNum settings
  timeoutRef      <- newExternalRef $ settingsReqTimeout settings
  (iReqE, iReqFire) <- newTriggerEvent
  let indexSel = fanMap iReqE -- Node request selector :: NodeReqSelector t
  indexEF <- newTriggerEvent
  let env = UnauthEnv {
          unauth'settings         = settingsRef
        , unauth'pauseEF          = (pauseE, pauseFire ())
        , unauth'resumeEF         = (resumeE, resumeFire ())
        , unauth'backEF           = (backE, backFire ())
        , unauth'loading          = loadingEF
        , unauth'langRef          = langRef
        , unauth'storeDir         = settingsStoreDir settings
        , unauth'alertsEF         = alertsEF
        , unauth'logsTrigger      = logsTrigger
        , unauth'logsNameSpaces   = nameSpaces
        , unauth'uiChan           = uiChan
        , unauth'authRef          = authRef
        , unauth'passModalEF      = passModalEF
        , unauth'passSetEF        = passSetEF
        , unauth'addrsArchive     = urlsArchive
        , unauth'inactiveAddrs    = inactiveUrls
        , unauth'activeAddrs      = actvieAddrsRef
        , unauth'indexConmap      = indexConmapRef
        , unauth'reqUrlNum        = reqUrlNumRef
        , unauth'actUrlNum        = actUrlNumRef
        , unauth'timeout          = timeoutRef
        , unauth'indexReqSel      = indexSel
        , unauth'indexReqFire     = iReqFire
        , unauth'activateIndexEF  = indexEF
        }
  flip runReaderT env $ do
    indexerNodeController socadrs
  pure env

runEnv :: (MonadBaseConstr t m, PlatformNatives, HasVersion)
  => RunCallbacks -> UnauthEnv t -> ReaderT (UnauthEnv t) (RetractT t m) a -> m a
runEnv cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ (snd . unauth'backEF) e
  liftIO $ writeIORef (runPauseCallback cbs) $ (snd . unauth'pauseEF) e
  liftIO $ writeIORef (runResumeCallback cbs) $ (snd . unauth'resumeEF) e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = void (retract . fst =<< getBackEventFire) >> ma
