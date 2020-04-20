{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Random.Class
import Control.Monad.Reader
import Data.IORef
import Data.Map.Strict (Map)
import Data.Time (NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.Connection
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (newTlsManagerWith, mkManagerSettings, newTlsManager)
import Network.TLS
import Network.TLS.Extra.Cipher
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Host.Class
import Servant.Client(BaseUrl)

import Ergvein.Crypto
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Filters.Loader
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Headers.Loader
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings (Settings(..), storeSettings, defaultIndexers)
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Worker.Height
import Ergvein.Wallet.Worker.Info

import qualified Control.Immortal as I
import qualified Data.IntMap.Strict as MI
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L

data Env t = Env {
  -- Unauth context's fields
  env'settings        :: !(ExternalRef t Settings)
, env'backEF          :: !(Event t (), IO ())
, env'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, env'langRef         :: !(ExternalRef t Language)
, env'storeDir        :: !Text
, env'alertsEF        :: (Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alert event and trigger
, env'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, env'logsNameSpaces  :: !(ExternalRef t [Text])
, env'uiChan          :: !(Chan (IO ()))
, env'passModalEF     :: !(Event t Int, Int -> IO ())
, env'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
-- Auth context
, env'authRef         :: !(ExternalRef t AuthInfo)
, env'logoutFire      :: !(IO ())
, env'activeCursRef   :: !(ExternalRef t ActiveCurrencies)
, env'manager         :: !(IORef Manager)
, env'headersStorage  :: !HeadersStorage
, env'filtersStorage  :: !FiltersStorage
, env'syncProgress    :: !(ExternalRef t SyncProgress)
, env'heightRef       :: !(ExternalRef t (Map Currency Integer))
, env'filtersSyncRef  :: !(ExternalRef t (Map Currency Bool))
, env'urlsArchive     :: !(ExternalRef t (S.Set BaseUrl))
, env'inactiveUrls    :: !(ExternalRef t (S.Set BaseUrl))
, env'activeUrls      :: !(ExternalRef t (Map BaseUrl (Maybe IndexerInfo)))
, env'reqUrlNum       :: !(ExternalRef t (Int, Int))
, env'actUrlNum       :: !(ExternalRef t Int)
, env'timeout         :: !(ExternalRef t NominalDiffTime)
, env'indexersEF      :: !(Event t (), IO ())
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir
  {-# INLINE getStoreDir #-}

instance Monad m => HasHeadersStorage (ErgveinM t m) where
  getHeadersStorage = asks env'headersStorage
  {-# INLINE getHeadersStorage #-}

instance Monad m => HasFiltersStorage (ErgveinM t m) where
  getFiltersStorage = asks env'filtersStorage
  {-# INLINE getFiltersStorage #-}

instance MonadIO m => HasClientManager (ErgveinM t m) where
  getClientMaganer = liftIO . readIORef =<< asks env'manager

instance MonadBaseConstr t m => MonadEgvLogger t (ErgveinM t m) where
  getLogsTrigger = asks env'logsTrigger
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = asks env'logsNameSpaces
  {-# INLINE getLogsNameSpacesRef #-}

instance MonadBaseConstr t m => MonadLocalized t (ErgveinM t m) where
  setLanguage lang = do
    langRef <- asks env'langRef
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks env'langRef
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks env'langRef
  {-# INLINE getLanguage #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives) => MonadFrontBase t (ErgveinM t m) where
  getSettings = readExternalRef =<< asks env'settings
  {-# INLINE getSettings #-}
  getSettingsD = externalRefDynamic =<< asks env'settings
  {-# INLINE getSettingsD #-}
  getLoadingWidgetTF = asks env'loading
  {-# INLINE getLoadingWidgetTF #-}
  toggleLoadingWidget reqE = do
    fire <- asks (snd . env'loading)
    langRef <- asks env'langRef
    performEvent_ $ ffor reqE $ \(b,lbl) -> liftIO $ do
      lang <- readExternalRef langRef
      fire (b,localizedShow lang lbl)
  {-# INLINE toggleLoadingWidget #-}
  loadingWidgetDyn reqD = do
    fire <- asks (snd . env'loading)
    langRef <- asks env'langRef
    performEvent_ $ ffor (updated reqD) $ \(b,lbl) -> liftIO $ do
      lang <- readExternalRef langRef
      fire (b,localizedShow lang lbl)
  {-# INLINE loadingWidgetDyn #-}
  getBackEventFire = asks env'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks env'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks env'langRef
  {-# INLINE getLangRef #-}
  isAuthorized = do
    authd <- getAuthInfoMaybe
    pure $ ffor authd $ \case
      Just _ -> True
      Nothing -> False
  {-# INLINE isAuthorized #-}
  getAuthInfoMaybe = (fmap . fmap) Just . externalRefDynamic =<< asks env'authRef
  {-# INLINE getAuthInfoMaybe #-}
  getAuthInfoRef = fmapExternalRef Just =<< asks env'authRef
  {-# INLINE getAuthInfoRef #-}
  setAuthInfo e = do
    authRef <- asks env'authRef
    fire <- asks env'logoutFire
    performEvent $ ffor e $ \case
      Nothing -> do
        logWrite "authed setAuthInfo: logout"
        setLastStorage Nothing
        liftIO fire
      Just v -> do
        logWrite "authed setAuthInfo: changing auth info"
        setLastStorage $ Just . _storage'walletName . _authInfo'storage $ v
        writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}
  getPasswordModalEF = asks env'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks env'passSetEF
  {-# INLINE getPasswordSetEF #-}
  requestPasssword reqE = do
    idE <- performEvent $ liftIO getRandom <$ reqE
    idD <- holdDyn 0 idE
    (_, modalF) <- asks env'passModalEF
    (setE, _) <- asks env'passSetEF
    performEvent_ $ fmap (liftIO . modalF) idE
    pure $ attachWithMaybe (\i' (i,mp) -> if i == i' then mp else Nothing) (current idD) setE
  updateSettings setE = do
    settingsRef <- asks env'settings
    performEvent $ ffor setE $ \s -> do
      writeExternalRef settingsRef s
      storeSettings s
  {-# INLINE updateSettings #-}
  getSettingsRef = asks env'settings
  {-# INLINE getSettingsRef #-}

instance MonadFrontBase t m => MonadFrontAuth t (ErgveinM t m) where
  getSyncProgressRef = asks env'syncProgress
  {-# INLINE getSyncProgressRef #-}
  getSyncProgress = externalRefDynamic =<< asks env'syncProgress
  {-# INLINE getSyncProgress #-}
  setSyncProgress ev = do
    ref <- asks env'syncProgress
    performEvent_ $ writeExternalRef ref <$> ev
  {-# INLINE setSyncProgress #-}
  getHeightRef = asks env'heightRef
  {-# INLINE getHeightRef #-}
  getFiltersSyncRef = asks env'filtersSyncRef
  {-# INLINE getFiltersSyncRef #-}
  getActiveCursRef = asks env'activeCursRef
  {-# INLINE getActiveCursRef #-}
  getAuthInfo = externalRefDynamic =<< asks env'authRef
  {-# INLINE getAuthInfo #-}
  getLoginD = (fmap . fmap) _authInfo'login . externalRefDynamic =<< asks env'authRef
  {-# INLINE getLoginD #-}

instance MonadBaseConstr t m => MonadAlertPoster t (ErgveinM t m) where
  postAlert e = do
    (_, fire) <- asks env'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . env'alertsEF)
  getAlertEventFire = asks env'alertsEF
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t (ErgveinM t m) where
  getEncryptedPrvStorage = fmap (_storage'encryptedPrvStorage . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getEncryptedPrvStorage #-}
  getAddressByCurIx cur i = do
    currMap <- fmap (_pubStorage'currencyPubStorages . _storage'pubStorage . _authInfo'storage) $ readExternalRef =<< asks env'authRef
    let mXPubKey = (MI.lookup i) . pubKeystore'external . _currencyPubStorage'pubKeystore =<< M.lookup cur currMap
    case mXPubKey of
      Nothing -> fail "NOT IMPLEMENTED" -- TODO: generate new address here
      Just xPubKey -> pure $ xPubExport (getCurrencyNetwork cur) (egvXPubKey xPubKey)
  {-# INLINE getAddressByCurIx #-}
  getWalletName = fmap (_storage'walletName . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getWalletName #-}
  getPubStorage = fmap (_storage'pubStorage . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getPubStorage #-}
  storeWallet e = do
    authInfo <- readExternalRef =<< asks env'authRef
    performEvent_ $ ffor e $ \_ -> do
      let storage = _authInfo'storage authInfo
      let eciesPubKey = _authInfo'eciesPubKey authInfo
      saveStorageToFile eciesPubKey storage
  {-# INLINE storeWallet #-}

-- | Execute action under authorized context or return the given value as result
-- if user is not authorized. Each time the login info changes and authInfo'isUpdate flag is set to 'False'
-- (user logs out or logs in) the widget is updated.
liftAuth :: MonadFrontBase t m => m a -> ErgveinM t m a -> m (Dynamic t a)
liftAuth ma0 ma = mdo
  mauthD <- getAuthInfoMaybe
  mauth0 <- sample . current $ mauthD
  (logoutE, logoutFire) <- newTriggerEvent
  let runAuthed auth = do
        -- Get refs from Unauth context
        backEF          <- getBackEventFire
        loading         <- getLoadingWidgetTF
        langRef         <- getLangRef
        storeDir        <- getStoreDir
        alertsEF        <- getAlertEventFire
        logsTrigger     <- getLogsTrigger
        logsNameSpaces  <- getLogsNameSpacesRef
        uiChan          <- getUiChan
        passModalEF     <- getPasswordModalEF
        passSetEF       <- getPasswordSetEF
        settingsRef     <- getSettingsRef

        -- Read settings to fill other refs
        settings        <- readExternalRef settingsRef

        -- MonadClient refs
        authRef         <- newExternalRef auth
        urlsArchive     <- newExternalRef $ S.fromList $ settingsPassiveUrls settings
        inactiveUrls    <- newExternalRef $ S.fromList $ settingsDeactivatedUrls settings
        activeUrlsRef   <- newExternalRef $ M.fromList $ fmap (,Nothing) $ settingsActiveUrls settings
        reqUrlNumRef    <- newExternalRef $ settingsReqUrlNum settings
        actUrlNumRef    <- newExternalRef $ settingsActUrlNum settings
        timeoutRef      <- newExternalRef $ settingsReqTimeout settings
        (indexersE, indexersF) <- newTriggerEvent

        -- Create data for Auth context
        managerRef      <- liftIO . newIORef =<< newTlsManager
        activeCursRef   <- newExternalRef $ settingsActiveCurrencies settings
        headersStore    <- liftIO $ runReaderT openHeadersStorage (settingsStoreDir settings)
        syncRef         <- newExternalRef Synced
        filtersStore    <- liftIO $ runReaderT openFiltersStorage (settingsStoreDir settings)
        heightRef       <- newExternalRef mempty
        fsyncRef        <- newExternalRef mempty

        -- headersLoader
        let env = Env
              settingsRef backEF loading langRef storeDir alertsEF logsTrigger logsNameSpaces uiChan passModalEF passSetEF
              authRef (logoutFire ()) activeCursRef managerRef headersStore filtersStore syncRef heightRef fsyncRef
              urlsArchive inactiveUrls activeUrlsRef reqUrlNumRef actUrlNumRef timeoutRef (indexersE, indexersF ())

        flip runReaderT env $ do -- Workers and other routines go here
          filtersLoader
          infoWorker
          heightAsking
        runOnUiThreadM $ runReaderT setupTlsManager env
        runReaderT (wrapped ma) env
  let
    ma0' = maybe ma0 runAuthed mauth0
    newAuthInfoE = ffilter isMauthUpdate $ updated mauthD
    redrawE = leftmost [newAuthInfoE, Nothing <$ logoutE]
  widgetHold ma0' $ ffor redrawE $ maybe ma0 runAuthed

isMauthUpdate :: Maybe AuthInfo -> Bool
isMauthUpdate mauth = case mauth of
  Nothing -> True
  Just auth -> not $ _authInfo'isUpdate auth

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma

wrapped :: MonadFrontBase t m => ErgveinM t m a -> ErgveinM t m a
wrapped ma = do
  storeWallet =<< getPostBuild
  ma

instance MonadBaseConstr t m => MonadClient t (ErgveinM t m) where
  getArchivedUrlsRef = asks env'urlsArchive
  {-# INLINE getArchivedUrlsRef #-}
  getActiveUrlsRef = asks env'activeUrls
  {-# INLINE getActiveUrlsRef #-}
  getInactiveUrlsRef = asks env'inactiveUrls
  {-# INLINE getInactiveUrlsRef #-}
  getInactiveUrlsD = externalRefDynamic =<< asks env'inactiveUrls
  {-# INLINE getInactiveUrlsD #-}
  getActiveUrlsNumRef = asks env'actUrlNum
  {-# INLINE getActiveUrlsNumRef #-}
  getRequiredUrlNumRef = asks env'reqUrlNum
  {-# INLINE getRequiredUrlNumRef #-}
  getRequestTimeoutRef = asks env'timeout
  {-# INLINE getRequestTimeoutRef #-}
  getIndexerInfoD = externalRefDynamic =<< asks env'activeUrls
  {-# INLINE getIndexerInfoD #-}
  getIndexerInfoEF = asks env'indexersEF
  {-# INLINE getIndexerInfoEF #-}
  refreshIndexerInfo e = do
    fire <- asks (snd . env'indexersEF)
    performEvent_ $ (liftIO fire) <$ e
  {-# INLINE refreshIndexerInfo #-}
  pingIndexer urlE = do
    mng <- getClientMaganer
    performEvent $ (pingIndexerIO mng) <$> urlE
  activateURL urlE = do
    actRef  <- asks env'activeUrls
    iaRef   <- asks env'inactiveUrls
    acrhRef <- asks env'urlsArchive
    setRef  <- asks env'settings
    mng     <- getClientMaganer
    performEvent $ ffor urlE $ \url -> do
      res <- pingIndexerIO mng url
      ias <- modifyExternalRef iaRef $ \us ->
        let us' = S.delete url us in (us', S.toList us')
      ars <- modifyExternalRef acrhRef $ \as ->
        let as' = S.delete url as in  (as', S.toList as')
      acs <- modifyExternalRef actRef $ \as ->
        let as' = uncurry M.insert res as in (as', M.keys as')
      s <- modifyExternalRef setRef $ \s -> let
        s' = s {
            settingsActiveUrls      = acs
          , settingsDeactivatedUrls = ias
          , settingsPassiveUrls     = ars
          }
        in (s', s')
      storeSettings s
      pure ()
  deactivateURL urlE = do
    actRef  <- asks env'activeUrls
    iaRef   <- asks env'inactiveUrls
    setRef  <- asks env'settings
    performEvent $ ffor urlE $ \url -> do
      acs <- modifyExternalRef actRef $ \as ->
        let as' = M.delete url as in (as', M.keys as')
      ias <- modifyExternalRef iaRef  $ \us ->
        let us' = S.insert url us in (us', S.toList us')
      s <- modifyExternalRef setRef $ \s -> let
        s' = s {
            settingsActiveUrls      = acs
          , settingsDeactivatedUrls = ias
          }
        in (s', s')
      storeSettings s
  forgetURL urlE = do
    actRef  <- asks env'activeUrls
    iaRef   <- asks env'inactiveUrls
    acrhRef <- asks env'urlsArchive
    setRef  <- asks env'settings
    performEvent $ ffor urlE $ \url -> do
      ias <- modifyExternalRef iaRef $ \us ->
        let us' = S.delete url us in (us', S.toList us')
      ars <- modifyExternalRef acrhRef $ \as ->
        let as' = S.delete url as in  (as', S.toList as')
      acs <- modifyExternalRef actRef $ \as ->
        let as' = M.delete url as in (as', M.keys as')
      s <- modifyExternalRef setRef $ \s -> let
        s' = s {
            settingsActiveUrls      = acs
          , settingsDeactivatedUrls = ias
          , settingsPassiveUrls     = ars
          }
        in (s', s')
      storeSettings s
  restoreDefaultIndexers reqE = do
    actRef  <- asks env'activeUrls
    iaRef   <- asks env'inactiveUrls
    acrhRef <- asks env'urlsArchive
    setRef  <- asks env'settings
    let defSet = S.fromList defaultIndexers
    performEvent $ ffor reqE $ const $ do
      ias <- modifyExternalRef iaRef $ \us ->
        let us' = us `S.difference` defSet in (us', S.toList us')
      ars <- modifyExternalRef acrhRef $ \as ->
        let as' = as `S.difference` defSet in  (as', S.toList as')
      acs <- modifyExternalRef actRef $ \as ->
        let as' = L.foldl' (\m u -> M.insert u Nothing m) as defaultIndexers
        in (as', M.keys as')
      s <- modifyExternalRef setRef $ \s -> let
        s' = s {
            settingsActiveUrls      = acs
          , settingsDeactivatedUrls = ias
          , settingsPassiveUrls     = ars
          }
        in (s', s')
      storeSettings s
      pure ()

pingIndexerIO :: MonadIO m => Manager -> BaseUrl -> m (BaseUrl, Maybe IndexerInfo)
pingIndexerIO mng url = liftIO $ do
  t0 <- getCurrentTime
  res <- runReaderT (getInfoEndpoint url ()) mng
  t1 <- getCurrentTime
  pure $ case res of
    Left _ -> (url, Nothing)
    Right (InfoResponse vals) -> let
      curmap = M.fromList $ fmap (\(ScanProgressItem cur sh ah) -> (cur, (sh, ah))) vals
      in (url, Just $ IndexerInfo curmap $ diffUTCTime t1 t0)

mkTlsSettings :: (MonadIO m, PlatformNatives) => m TLSSettings
mkTlsSettings = do
  store <- readSystemCertificates
  pure $ TLSSettings $ defParams {
      clientShared = (clientShared defParams) {
        sharedCAStore = store
      }
    , clientSupported = def {
        supportedCiphers = ciphersuite_strong
      }
    }
  where
    defParams = defaultParamsClient "localhost" ""

setupTlsManager :: (MonadIO m, MonadReader (Env t) m, PlatformNatives) => m ()
setupTlsManager = do
  e <- ask
  sett <- mkTlsSettings
  liftIO $ do
    manager <- newTlsManagerWith $ mkManagerSettings sett Nothing
    writeIORef (env'manager e) manager
