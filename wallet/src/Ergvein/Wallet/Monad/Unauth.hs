module Ergvein.Wallet.Monad.Unauth
  (
    UnauthEnv(..)
  , newEnv
  , runEnv
  ) where

import Control.Concurrent.Chan
import Control.Monad.Random.Class
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import Data.Time(NominalDiffTime)
import Ergvein.Index.Client
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Settings
import Ergvein.Types.Storage
import Ergvein.Wallet.Storage.Util
import Network.HTTP.Client hiding (Proxy)
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Servant.Client(BaseUrl)

import qualified Data.Set as S

data UnauthEnv t = UnauthEnv {
  unauth'settings        :: !(ExternalRef t Settings)
, unauth'backEF          :: !(Event t (), IO ())
, unauth'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, unauth'langRef         :: !(ExternalRef t Language)
, unauth'storeDir        :: !Text
, unauth'alertsEF        :: !(Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alerts event and trigger
, unauth'logsTrigger     :: !(Event t LogEntry, LogEntry -> IO ())
, unauth'logsNameSpaces  :: !(ExternalRef t [Text])
, unauth'uiChan          :: !(Chan (IO ()))
, unauth'authRef         :: !(ExternalRef t (Maybe AuthInfo))
, unauth'passModalEF     :: !(Event t Int, Int -> IO ())
, unauth'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
, unauth'urls            :: !(ExternalRef t (S.Set BaseUrl))
, unauth'urlNum          :: !(ExternalRef t (Int, Int))
, unauth'timeout         :: !(ExternalRef t NominalDiffTime)
, unauth'headersStorage  :: !HeadersStorage
, unauth'manager         :: !Manager
}

type UnauthM t m = ReaderT (UnauthEnv t) m

instance Monad m => HasStoreDir (UnauthM t m) where
  getStoreDir = asks unauth'storeDir
  {-# INLINE getStoreDir #-}

instance Monad m => HasHeadersStorage (UnauthM t m) where
  getHeadersStorage = asks unauth'headersStorage
  {-# INLINE getHeadersStorage #-}

instance MonadIO m => HasClientManager (UnauthM t m) where
  getClientMaganer = asks unauth'manager

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

instance MonadBaseConstr t m => MonadClient t (UnauthM t m) where
  setRequiredUrlNum numE = do
    numRef <- asks unauth'urlNum
    performEvent_ $ writeExternalRef numRef <$> numE

  getRequiredUrlNum reqE = do
    numRef <- asks unauth'urlNum
    performEvent $ readExternalRef numRef <$ reqE

  getUrlList reqE = do
    urlsRef <- asks unauth'urls
    performEvent $ ffor reqE $ const $ liftIO $ S.elems <$> readExternalRef urlsRef

  addUrls urlsE = do
    urlsRef <- asks unauth'urls
    performEvent_ $ ffor urlsE $ \urls ->
      modifyExternalRef urlsRef (\s -> (S.union (S.fromList urls) s, ()) )

  invalidateUrls urlsE = do
    urlsRef <- asks unauth'urls
    performEvent_ $ ffor urlsE $ \urls ->
      modifyExternalRef urlsRef (\s -> (S.difference s (S.fromList urls), ()) )
  getUrlsRef = asks unauth'urls
  getRequiredUrlNumRef = asks unauth'urlNum
  getRequestTimeoutRef = asks unauth'timeout

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives) => MonadFrontBase t (UnauthM t m) where
  getSettings = readExternalRef =<< asks unauth'settings
  {-# INLINE getSettings #-}
  getLoadingWidgetTF = asks unauth'loading
  {-# INLINE getLoadingWidgetTF #-}
  toggleLoadingWidget reqE = do
    fire <- asks (snd . unauth'loading)
    langRef <- asks unauth'langRef
    performEvent_ $ ffor reqE $ \(b,lbl) -> liftIO $ do
      lang <- readExternalRef langRef
      fire (b,localizedShow lang lbl)
  {-# INLINE toggleLoadingWidget #-}
  loadingWidgetDyn reqD = do
    fire <- asks (snd . unauth'loading)
    langRef <- asks unauth'langRef
    performEvent_ $ ffor (updated reqD) $ \(b,lbl) -> liftIO $ do
      lang <- readExternalRef langRef
      fire (b,localizedShow lang lbl)
  {-# INLINE loadingWidgetDyn #-}
  getBackEventFire = asks unauth'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks unauth'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks unauth'langRef
  {-# INLINE getLangRef #-}
  isAuthorized = do
    authd <- getAuthInfoMaybe
    pure $ ffor authd $ \case
      Just _ -> True
      Nothing -> False
  {-# INLINE isAuthorized #-}
  getAuthInfoMaybe = externalRefDynamic =<< asks unauth'authRef
  {-# INLINE getAuthInfoMaybe #-}
  getAuthInfoRef = asks unauth'authRef
  {-# INLINE getAuthInfoRef #-}
  setAuthInfo e = do
    authRef <- asks unauth'authRef
    performEvent $ ffor e $ \v -> do
      logWrite "unauthed setAuthInfo: setting info"
      setLastStorage $ storage'walletName . authInfo'storage <$> v
      writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}
  getPasswordModalEF = asks unauth'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks unauth'passSetEF
  {-# INLINE getPasswordSetEF #-}
  requestPasssword reqE = do
    idE <- performEvent $ liftIO getRandom <$ reqE
    idD <- holdDyn 0 idE
    (_, modalF) <- asks unauth'passModalEF
    (setE, _) <- asks unauth'passSetEF
    performEvent_ $ fmap (liftIO . modalF) idE
    pure $ attachWithMaybe (\i' (i,mp) -> if i == i' then mp else Nothing) (current idD) setE
  updateSettings setE = do
    settingsRef <- asks unauth'settings
    performEvent_ $ ffor setE $ \s -> do
      writeExternalRef settingsRef s
      storeSettings s
  {-# INLINE updateSettings #-}
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

newEnv :: (Reflex t, TriggerEvent t m, MonadIO m)
  => Settings
  -> Chan (IO ()) -- UI callbacks channel
  -> m (UnauthEnv t)
newEnv settings uiChan = do
  settingsRef <- newExternalRef settings
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  alertsEF <- newTriggerEvent
  passSetEF <- newTriggerEvent
  passModalEF <- newTriggerEvent
  authRef <- newExternalRef Nothing
  langRef <- newExternalRef $ settingsLang settings
  re <- newRetractEnv
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []
  manager <- liftIO $ newManager defaultManagerSettings
  urls <- newExternalRef $ S.fromList $ settingsDefUrls settings
  urlNum <- newExternalRef $ settingsDefUrlNum settings
  timeout <- newExternalRef $ settingsReqTimeout settings
  hst <- liftIO $ runReaderT openHeadersStorage (settingsStoreDir settings)
  pure UnauthEnv {
      unauth'settings  = settingsRef
    , unauth'backEF    = (backE, backFire ())
    , unauth'loading   = loadingEF
    , unauth'langRef   = langRef
    , unauth'storeDir  = settingsStoreDir settings
    , unauth'alertsEF  = alertsEF
    , unauth'logsTrigger = logsTrigger
    , unauth'logsNameSpaces = nameSpaces
    , unauth'uiChan = uiChan
    , unauth'authRef = authRef
    , unauth'passModalEF = passModalEF
    , unauth'passSetEF = passSetEF
    , unauth'urls = urls
    , unauth'urlNum = urlNum
    , unauth'timeout = timeout
    , unauth'manager = manager
    , unauth'headersStorage = hst
    }

runEnv :: (MonadBaseConstr t m, PlatformNatives)
  => RunCallbacks -> UnauthEnv t -> ReaderT (UnauthEnv t) (RetractT t m) a -> m a
runEnv cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ (snd . unauth'backEF) e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = void (retract . fst =<< getBackEventFire) >> ma
