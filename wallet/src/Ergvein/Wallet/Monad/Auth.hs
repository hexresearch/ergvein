{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent
import Control.Concurrent.Chan (Chan)
import Control.Lens
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Text as T
import Data.Time (NominalDiffTime)
import Network.Connection
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS (newTlsManagerWith, mkManagerSettings)
import Network.Socket (SockAddr)
import Network.TLS
import Network.TLS.Extra.Cipher
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Servant.Client(BaseUrl)
import System.Directory

import Ergvein.Crypto as Crypto
import Ergvein.Index.Client
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Wallet.Filters.Loader
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Settings (Settings(..))
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Version
import Ergvein.Wallet.Worker.Fees
import Ergvein.Wallet.Worker.Height
import Ergvein.Wallet.Worker.IndexersNetworkActualization
import Ergvein.Wallet.Worker.Node

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Network.Haskoin.Block as HS (BlockHeight)

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
, env'passModalEF     :: !(Event t (Int, Text), (Int, Text) -> IO ())
, env'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
-- Auth context
, env'authRef         :: !(ExternalRef t AuthInfo)
, env'logoutFire      :: !(IO ())
, env'activeCursRef   :: !(ExternalRef t (S.Set Currency))
, env'manager         :: !(MVar Manager)
, env'filtersStorage  :: !FiltersStorage
, env'filtersHeights  :: !(ExternalRef t (Map Currency HS.BlockHeight))
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
, env'nodeConsRef     :: !(ExternalRef t (ConnMap t))
, env'nodeReqSelector :: !(RequestSelector t)
, env'nodeReqFire     :: !(Map Currency (Map SockAddr NodeMessage) -> IO ())
, env'feesStore       :: !(ExternalRef t (Map Currency FeeBundle))
, env'storeMutex      :: !(MVar ())
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir
  {-# INLINE getStoreDir #-}

instance Monad m => HasFiltersStorage t (ErgveinM t m) where
  getFiltersStorage = asks env'filtersStorage
  {-# INLINE getFiltersStorage #-}
  getFiltersHeightRef = asks env'filtersHeights
  {-# INLINE getFiltersHeightRef #-}

instance MonadIO m => HasClientManager (ErgveinM t m) where
  getClientManager = liftIO . readMVar =<< asks env'manager

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

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives, HasVersion) => MonadFrontBase t (ErgveinM t m) where
  getLoadingWidgetTF = asks env'loading
  {-# INLINE getLoadingWidgetTF #-}
  getBackEventFire = asks env'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks env'uiChan
  {-# INLINE getUiChan #-}
  getLangRef = asks env'langRef
  {-# INLINE getLangRef #-}
  getAuthInfoMaybeRef = fmapExternalRef Just =<< asks env'authRef
  {-# INLINE getAuthInfoMaybeRef #-}
  getPasswordModalEF = asks env'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks env'passSetEF
  {-# INLINE getPasswordSetEF #-}
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

instance MonadBaseConstr t m => MonadHasSettings t (ErgveinM t m) where
  getSettingsRef = asks env'settings
  {-# INLINE getSettingsRef #-}

instance MonadFrontBase t m => MonadFrontAuth t (ErgveinM t m) where
  getSyncProgressRef = asks env'syncProgress
  {-# INLINE getSyncProgressRef #-}
  getHeightRef = asks env'heightRef
  {-# INLINE getHeightRef #-}
  getFiltersSyncRef = asks env'filtersSyncRef
  {-# INLINE getFiltersSyncRef #-}
  getActiveCursRef = asks env'activeCursRef
  {-# INLINE getActiveCursRef #-}
  getAuthInfoRef = asks env'authRef
  {-# INLINE getAuthInfoRef #-}
  getNodeConnRef = asks env'nodeConsRef
  {-# INLINE getNodeConnRef #-}
  getNodeRequestSelector = asks env'nodeReqSelector
  {-# INLINE getNodeRequestSelector #-}
  getFeesRef = asks env'feesStore
  {-# INLINE getFeesRef #-}
  getNodeReqFire = asks env'nodeReqFire
  {-# INLINE getNodeReqFire #-}

instance MonadBaseConstr t m => MonadClient t (ErgveinM t m) where
  getArchivedUrlsRef = asks env'urlsArchive
  {-# INLINE getArchivedUrlsRef #-}
  getActiveUrlsRef = asks env'activeUrls
  {-# INLINE getActiveUrlsRef #-}
  getInactiveUrlsRef = asks env'inactiveUrls
  {-# INLINE getInactiveUrlsRef #-}
  getActiveUrlsNumRef = asks env'actUrlNum
  {-# INLINE getActiveUrlsNumRef #-}
  getRequiredUrlNumRef = asks env'reqUrlNum
  {-# INLINE getRequiredUrlNumRef #-}
  getRequestTimeoutRef = asks env'timeout
  {-# INLINE getRequestTimeoutRef #-}
  getIndexerInfoEF = asks env'indexersEF
  {-# INLINE getIndexerInfoEF #-}

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
    let mXPubKey = (flip (V.!?) i) . pubKeystore'external . _currencyPubStorage'pubKeystore =<< M.lookup cur currMap
    case mXPubKey of
      Nothing -> fail "NOT IMPLEMENTED" -- TODO: generate new address here
      Just (EgvPubKeyBox key _ _) ->
        let k = case key of
              ErgXPubKey k' _ -> k'
              BtcXPubKey k' _ -> k'
        in pure $ xPubExport (getCurrencyNetwork cur) k
  {-# INLINE getAddressByCurIx #-}
  getWalletName = fmap (_storage'walletName . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getWalletName #-}
  getPubStorage = fmap (_storage'pubStorage . _authInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getPubStorage #-}
  getPubStorageD = do
    authInfoD <- externalRefDynamic =<< asks env'authRef
    pure $ ffor authInfoD $ \ai -> ai ^. authInfo'storage. storage'pubStorage
  {-# INLINE getPubStorageD #-}
  storeWallet caller e = do
    ref <-  asks env'authRef
    performEvent_ $ ffor e $ \_ -> do
        authInfo <- readExternalRef ref
        let storage = _authInfo'storage authInfo
        let eciesPubKey = _authInfo'eciesPubKey authInfo
        saveStorageToFile caller eciesPubKey storage
  {-# INLINE storeWallet #-}

  modifyPubStorage caller fe = do
    authRef   <- asks env'authRef
    mutex     <- asks env'storeMutex
    storeDir  <- asks env'storeDir
    performEvent $ ffor fe $ \f -> do
      mai <- modifyExternalRefMaybe authRef $ \ai ->
        let mps' = f (ai ^. authInfo'storage . storage'pubStorage)
        in (\a -> (a, a)) . (\ps' -> ai & authInfo'storage . storage'pubStorage .~ ps') <$> mps'
      liftIO $ storeWalletIO caller storeDir mutex mai
  {-# INLINE modifyPubStorage #-}
  getStoreMutex = asks env'storeMutex
  {-# INLINE getStoreMutex #-}

storeWalletIO :: PlatformNatives => Text -> Text -> MVar () -> Maybe AuthInfo -> IO ()
storeWalletIO caller storeDir mutex mai = case mai of
  Nothing -> pure ()
  Just ai -> do
    let storage = _authInfo'storage ai
    let eciesPubKey = _authInfo'eciesPubKey ai
    withMVar mutex $ const $ flip runReaderT storeDir $ saveStorageToFile caller eciesPubKey storage

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
        (reqE, reqFire) <- newTriggerEvent
        let sel = fanMap reqE -- Node request selector :: RequestSelector t
        let ps = auth ^. authInfo'storage . storage'pubStorage

        managerRef      <- liftIO newEmptyMVar
        activeCursRef   <- newExternalRef mempty
        syncRef         <- newExternalRef Synced
        filtersStore    <- liftIO $ runReaderT openFiltersStorage (settingsStoreDir settings)
        filtersHeights  <- newExternalRef mempty
        heightRef       <- newExternalRef (fmap (maybe 0 fromIntegral . _currencyPubStorage'height) . _pubStorage'currencyPubStorages $ ps)
        fsyncRef        <- newExternalRef mempty
        consRef         <- newExternalRef mempty
        feesRef         <- newExternalRef mempty
        storeMvar       <- liftIO $ newMVar ()
        let env = Env {
                env'settings = settingsRef
              , env'backEF = backEF
              , env'loading = loading
              , env'langRef = langRef
              , env'storeDir = storeDir
              , env'alertsEF = alertsEF
              , env'logsTrigger = logsTrigger
              , env'logsNameSpaces = logsNameSpaces
              , env'uiChan = uiChan
              , env'passModalEF = passModalEF
              , env'passSetEF = passSetEF
              , env'authRef = authRef
              , env'logoutFire = logoutFire ()
              , env'activeCursRef = activeCursRef
              , env'manager = managerRef
              , env'filtersStorage = filtersStore
              , env'filtersHeights = filtersHeights
              , env'syncProgress = syncRef
              , env'heightRef = heightRef
              , env'filtersSyncRef = fsyncRef
              , env'urlsArchive = urlsArchive
              , env'inactiveUrls = inactiveUrls
              , env'activeUrls = activeUrlsRef
              , env'reqUrlNum = reqUrlNumRef
              , env'actUrlNum = actUrlNumRef
              , env'timeout = timeoutRef
              , env'indexersEF = (indexersE, indexersF ())
              , env'nodeConsRef = consRef
              , env'nodeReqSelector = sel
              , env'nodeReqFire = reqFire
              , env'feesStore = feesRef
              , env'storeMutex = storeMvar
              }
        runOnUiThreadM $ runReaderT setupTlsManager env

        flip runReaderT env $ do -- Workers and other routines go here
          when isAndroid (deleteTmpFiles storeDir)
          initFiltersHeights filtersHeights
          scanner
          bctNodeController
          filtersLoader
          heightAsking
          indexersNetworkActualizationWorker
          feesWorker
          pure ()
        runReaderT (wrapped "liftAuth" ma) env
  let
    ma0' = maybe ma0 runAuthed mauth0
    newAuthInfoE = ffilter isMauthUpdate $ updated mauthD
    redrawE = leftmost [newAuthInfoE, Nothing <$ logoutE]
  widgetHold ma0' $ ffor redrawE $ maybe ma0 runAuthed

-- | Query initial values for filters heights and write down them to the external ref
initFiltersHeights :: MonadFront t m => ExternalRef t (Map Currency HS.BlockHeight) -> m ()
initFiltersHeights ref = do
  ps <- getPubStorage
  let curs =  _pubStorage'activeCurrencies ps
  scms <- flip traverse curs $ \cur -> do
    h <- getFiltersHeight cur
    pure (cur, h)
  writeExternalRef ref $ M.fromList scms

isMauthUpdate :: Maybe AuthInfo -> Bool
isMauthUpdate mauth = case mauth of
  Nothing -> True
  Just auth -> not $ _authInfo'isUpdate auth

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma

wrapped :: MonadFrontBase t m => Text -> ErgveinM t m a -> ErgveinM t m a
wrapped caller ma = do
  storeWallet clr =<< getPostBuild
  buildE <- getPostBuild
  ac <- _pubStorage'activeCurrencies <$> getPubStorage
  void . updateActiveCurs $ fmap (\cl -> const (S.fromList cl)) $ ac <$ buildE
  ma
  where clr = caller <> ":" <> "wrapped"

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
    putMVar (env'manager e) manager

-- Deletes files created with 'atomicWriteFile' from specified directiry
deleteTmpFiles :: MonadIO m => Text -> m ()
deleteTmpFiles dir = liftIO $ do
  entries <- listDirectory $ T.unpack dir
  traverse_ removeFile $ L.filter isTmpFile entries
  where isTmpFile filePath = "atomic" `L.isPrefixOf` filePath && ".write" `L.isSuffixOf` filePath
