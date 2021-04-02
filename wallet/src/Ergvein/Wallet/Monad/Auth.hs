{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}

module Ergvein.Wallet.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.STM
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Fixed
import Data.Text as T
import Data.Time (NominalDiffTime)
import Network.Socket
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef

import Ergvein.Node.Constants
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Types.Transaction (BlockHeight)
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node
import Ergvein.Wallet.Settings (Settings(..))
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Version

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

data Env t = Env {
  -- Unauth context's fields
  env'settings        :: !(ExternalRef t Settings)
, env'pauseEF         :: !(Event t (), IO ())
, env'resumeEF        :: !(Event t (), IO ())
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
, env'filtersHeights  :: !(ExternalRef t (Map Currency BlockHeight))
, env'statusUpdates    :: !(ExternalRef t (Map Currency StatusUpdate))
, env'filtersSyncRef  :: !(ExternalRef t (Map Currency Bool))
, env'nodeConsRef     :: !(ExternalRef t (ConnMap t))
, env'nodeReqSelector :: !(NodeReqSelector t)
, env'nodeReqFire     :: !(Map Currency (Map SockAddr NodeMessage) -> IO ())
, env'feesStore       :: !(ExternalRef t (Map Currency FeeBundle))
, env'storeMutex      :: !(MVar ())
, env'storeChan       :: !(TChan (Text, AuthInfo))
, env'ratesRef        :: !(ExternalRef t (Map Currency (Map Fiat Centi)))
-- Client context
, env'addrsArchive    :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, env'inactiveAddrs   :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, env'activeAddrs     :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, env'indexConmap     :: !(ExternalRef t (Map ErgveinNodeAddr (IndexerConnection t)))
, env'indexStatus     :: !(ExternalRef t (Map ErgveinNodeAddr IndexerStatus))
, env'reqUrlNum       :: !(ExternalRef t (Int, Int))
, env'actUrlNum       :: !(ExternalRef t Int)
, env'timeout         :: !(ExternalRef t NominalDiffTime)
, env'indexReqSel     :: !(IndexReqSelector t)
, env'indexReqFire    :: !(Map ErgveinNodeAddr IndexerMsg -> IO ())
, env'activateIndexEF :: !(Event t [ErgveinNodeAddr], [ErgveinNodeAddr] -> IO ())
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir
  {-# INLINE getStoreDir #-}

instance MonadIO m => MonadHasUI (ErgveinM t m) where
  getUiChan = asks env'uiChan
  {-# INLINE getUiChan #-}

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
  getPauseEventFire = asks env'pauseEF
  {-# INLINE getPauseEventFire #-}
  getResumeEventFire = asks env'resumeEF
  {-# INLINE getResumeEventFire #-}
  getBackEventFire = asks env'backEF
  {-# INLINE getBackEventFire #-}
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
  getStatusUpdRef = asks env'statusUpdates
  {-# INLINE getStatusUpdRef #-}
  getFiltersSyncRef = asks env'filtersSyncRef
  {-# INLINE getFiltersSyncRef #-}
  getActiveCursRef = asks env'activeCursRef
  {-# INLINE getActiveCursRef #-}
  getAuthInfoRef = asks env'authRef
  {-# INLINE getAuthInfoRef #-}
  getNodeConnRef = asks env'nodeConsRef
  {-# INLINE getNodeConnRef #-}
  getNodeNodeReqSelector = asks env'nodeReqSelector
  {-# INLINE getNodeNodeReqSelector #-}
  getFeesRef = asks env'feesStore
  {-# INLINE getFeesRef #-}
  getRatesRef = asks env'ratesRef
  {-# INLINE getRatesRef #-}
  getNodeReqFire = asks env'nodeReqFire
  {-# INLINE getNodeReqFire #-}

instance MonadBaseConstr t m => MonadIndexClient t (ErgveinM t m) where
  getActiveAddrsRef = asks env'activeAddrs
  {-# INLINE getActiveAddrsRef #-}
  getArchivedAddrsRef = asks env'addrsArchive
  {-# INLINE getArchivedAddrsRef #-}
  getActiveConnsRef = asks env'indexConmap
  {-# INLINE getActiveConnsRef #-}
  getStatusConnsRef = asks env'indexStatus
  {-# INLINE getStatusConnsRef #-}
  getInactiveAddrsRef = asks env'inactiveAddrs
  {-# INLINE getInactiveAddrsRef #-}
  getActiveUrlsNumRef = asks env'actUrlNum
  {-# INLINE getActiveUrlsNumRef #-}
  getRequiredUrlNumRef = asks env'reqUrlNum
  {-# INLINE getRequiredUrlNumRef #-}
  getRequestTimeoutRef = asks env'timeout
  {-# INLINE getRequestTimeoutRef #-}
  getIndexReqSelector = asks env'indexReqSel
  {-# INLINE getIndexReqSelector #-}
  getIndexReqFire = asks env'indexReqFire
  {-# INLINE getIndexReqFire #-}
  getActivationEF = asks env'activateIndexEF
  {-# INLINE getActivationEF #-}

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
    performEvent $ ffor e $ \_ -> do
        authInfo <- readExternalRef ref
        let storage = _authInfo'storage authInfo
        let eciesPubKey = _authInfo'eciesPubKey authInfo
        saveStorageToFile caller eciesPubKey storage
  {-# INLINE storeWallet #-}

  modifyPubStorage caller fe = do
    authRef   <- asks env'authRef
    chan      <- asks env'storeChan
    performEvent $ ffor fe $ \f -> do
      mai <- modifyExternalRefMaybe authRef $ \ai ->
        let mps' = f (ai ^. authInfo'storage . storage'pubStorage)
        in (\a -> (a, a)) . (\ps' -> ai & authInfo'storage . storage'pubStorage .~ ps') <$> mps'
      liftIO $ atomically $ traverse_ (writeTChan chan . (caller, )) mai
  {-# INLINE modifyPubStorage #-}
  getStoreMutex = asks env'storeMutex
  {-# INLINE getStoreMutex #-}
  getStoreChan = asks env'storeChan
  {-# INLINE getStoreChan #-}

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
        pauseEF         <- getPauseEventFire
        resumeEF        <- getResumeEventFire
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

        authRef         <- newExternalRef auth

        -- MonadClient refs
        urlsArchive     <- getArchivedAddrsRef
        inactiveUrls    <- getInactiveAddrsRef
        actvieAddrsRef  <- getActiveAddrsRef
        indexConmapRef  <- getActiveConnsRef
        indexStatusRef  <- getStatusConnsRef
        reqUrlNumRef    <- getRequiredUrlNumRef
        actUrlNumRef    <- getActiveUrlsNumRef
        timeoutRef      <- getRequestTimeoutRef
        iReqFire        <- getIndexReqFire
        indexSel        <- getIndexReqSelector  -- Node request selector :: NodeReqSelector t
        indexEF         <- getActivationEF

        -- Create data for Auth context
        (nReqE, nReqFire) <- newTriggerEvent
        let nodeSel = fanMap nReqE -- Node request selector :: NodeReqSelector t


        activeCursRef   <- newExternalRef mempty
        statRef         <- newExternalRef mempty
        filtersHeights  <- newExternalRef mempty
        fsyncRef        <- newExternalRef mempty
        consRef         <- newExternalRef mempty
        feesRef         <- newExternalRef mempty
        ratesRef        <- newExternalRef mempty
        storeMutex      <- liftIO $ newMVar ()
        storeChan       <- liftIO newTChanIO
        let env = Env {
                env'settings = settingsRef
              , env'pauseEF = pauseEF
              , env'resumeEF = resumeEF
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
              , env'filtersHeights = filtersHeights
              , env'statusUpdates = statRef
              , env'filtersSyncRef = fsyncRef
              , env'nodeConsRef = consRef
              , env'nodeReqSelector = nodeSel
              , env'nodeReqFire = nReqFire
              , env'feesStore = feesRef
              , env'storeMutex = storeMutex
              , env'storeChan = storeChan
              , env'ratesRef  = ratesRef

              , env'addrsArchive = urlsArchive
              , env'inactiveAddrs = inactiveUrls
              , env'activeAddrs = actvieAddrsRef
              , env'indexConmap = indexConmapRef
              , env'indexStatus = indexStatusRef
              , env'reqUrlNum = reqUrlNumRef
              , env'actUrlNum = actUrlNumRef
              , env'timeout = timeoutRef
              , env'indexReqSel = indexSel
              , env'indexReqFire = iReqFire
              , env'activateIndexEF = indexEF
              }
        runReaderT (wrapped "liftAuth" ma) env
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

wrapped :: MonadFrontBase t m => Text -> ErgveinM t m a -> ErgveinM t m a
wrapped caller ma = do
  void $ storeWallet clr =<< getPostBuild
  buildE <- getPostBuild
  ac <- _pubStorage'activeCurrencies <$> getPubStorage
  void . updateActiveCurs $ fmap (\cl -> const (S.fromList cl)) $ ac <$ buildE
  ma
  where clr = caller <> ":" <> "wrapped"
