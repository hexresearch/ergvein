{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Random.Class
import Control.Monad.Reader
import Data.Functor (void)
import Data.List (permutations)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text, unpack)
import Data.Maybe
import Data.Time (NominalDiffTime)
import Ergvein.Crypto
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Headers.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Unauth
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings (Settings(..), storeSettings)
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Storage.Util
import Network.Haskoin.Address
import Network.HTTP.Client hiding (Proxy)
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Servant.Client(BaseUrl)

import qualified Data.IntMap.Strict as MI
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.List as L

data Env t = Env {
  env'settings        :: !(ExternalRef t Settings)
, env'backEF          :: !(Event t (), IO ())
, env'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, env'langRef         :: !(ExternalRef t Language)
, env'authRef         :: !(ExternalRef t (Maybe AuthInfo))
, env'logoutFire      :: !(IO ())
, env'storeDir        :: !Text
, env'alertsEF        :: (Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alert event and trigger
, env'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, env'logsNameSpaces  :: !(ExternalRef t [Text])
, env'uiChan          :: !(Chan (IO ()))
, env'passModalEF     :: !(Event t Int, Int -> IO ())
, env'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
, env'urls            :: !(ExternalRef t (S.Set BaseUrl))
, env'urlNum          :: !(ExternalRef t (Int, Int))
, env'timeout         :: !(ExternalRef t NominalDiffTime)
, env'manager         :: !Manager
, env'headersStorage  :: !HeadersStorage
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir
  {-# INLINE getStoreDir #-}

instance Monad m => HasHeadersStorage (ErgveinM t m) where
  getHeadersStorage = asks env'headersStorage
  {-# INLINE getHeadersStorage #-}

instance MonadIO m => HasClientManager (ErgveinM t m) where
  getClientMaganer = asks env'manager

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
  getAuthInfoMaybe = externalRefDynamic =<< asks env'authRef
  {-# INLINE getAuthInfoMaybe #-}
  getAuthInfoRef = asks env'authRef
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
        setLastStorage $ Just . storage'walletName . authInfo'storage $ v
        writeExternalRef authRef $ Just v
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
    performEvent_ $ ffor setE $ \s -> do
      writeExternalRef settingsRef s
      storeSettings s
  {-# INLINE updateSettings #-}
  getSettingsRef = asks env'settings
  {-# INLINE getSettingsRef #-}

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
  getEncryptedPrivateStorage = fmap (storage'encryptedPrivateStorage . authInfo'storage . guardit) $ readExternalRef =<< asks env'authRef
    where
      guardit Nothing = error "getEncryptedWallet impossible: no auth in authed context!"
      guardit (Just a) = a
  {-# INLINE getEncryptedPrivateStorage #-}
  getAddressByCurIx cur i = do
    currMap <- fmap (storage'publicKeys . authInfo'storage . guardit) $ readExternalRef =<< asks env'authRef
    let mXPubKey = (MI.lookup i) . egvPubKeyсhain'external =<< M.lookup cur currMap
    case mXPubKey of
      Nothing -> fail "NOT IMPLEMENTED" -- TODO: generate new address here
      Just xPubKey -> pure $ xPubExport (getCurrencyNetwork cur) (egvXPubKey xPubKey)
    where
      guardit Nothing = error "getAddressByCurIx impossible: no auth in authed context!"
      guardit (Just a) = a
  {-# INLINE getAddressByCurIx #-}
  getWalletName = fmap (storage'walletName . authInfo'storage . guardit) $ readExternalRef =<< asks env'authRef
    where
      guardit Nothing = error "getWalletName impossible: no auth in authed context!"
      guardit (Just a) = a
  {-# INLINE getWalletName #-}
  storeWallet e = do
    authInfo <- fmap guardit $ readExternalRef =<< asks env'authRef
    performEvent_ $ ffor e $ \_ -> do
      let storage = authInfo'storage authInfo
      let eciesPubKey = authInfo'eciesPubKey authInfo
      saveStorageToFile eciesPubKey storage
    where
      guardit Nothing = error "storeWallet impossible: no auth in authed context!"
      guardit (Just a) = a
  {-# INLINE storeWallet #-}

-- | Execute action under authorized context or return the given value as result
-- is user is not authorized. Each time the login info changes (user logs out or logs in)
-- the widget is updated.
liftAuth :: MonadFrontBase t m => m a -> ErgveinM t m a -> m (Dynamic t a)
liftAuth ma0 ma = mdo
  mauthD <- getAuthInfoMaybe
  mauth0 <- sample . current $ mauthD
  (logoutE, logoutFire) <- newTriggerEvent
  let runAuthed auth = do
        backEF          <- getBackEventFire
        loading         <- getLoadingWidgetTF
        langRef         <- getLangRef
        authRef         <- getAuthInfoRef
        storeDir        <- getStoreDir
        alertsEF        <- getAlertEventFire
        logsTrigger     <- getLogsTrigger
        logsNameSpaces  <- getLogsNameSpacesRef
        uiChan          <- getUiChan
        passModalEF     <- getPasswordModalEF
        passSetEF       <- getPasswordSetEF
        settingsRef     <- getSettingsRef
        settings        <- readExternalRef settingsRef
        urlsRef         <- getUrlsRef
        urlNumRef       <- getRequiredUrlNumRef
        timeoutRef      <- getRequestTimeoutRef
        manager         <- getClientMaganer
        hst             <- getHeadersStorage
        a <- runReaderT (wrapped ma) $ Env
          settingsRef backEF loading langRef authRef (logoutFire ()) storeDir alertsEF
          logsTrigger logsNameSpaces uiChan passModalEF passSetEF urlsRef urlNumRef timeoutRef manager hst
        pure a
  let
    ma0' = maybe ma0 runAuthed mauth0
    redrawE = leftmost [updated mauthD, Nothing <$ logoutE]
  widgetHold ma0' $ ffor redrawE $ maybe ma0 runAuthed

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma

wrapped :: MonadFrontBase t m => ErgveinM t m a -> ErgveinM t m a
wrapped ma = do
  storeWallet =<< getPostBuild
  ma

instance MonadBaseConstr t m => MonadClient t (ErgveinM t m) where
  setRequiredUrlNum numE = do
    numRef <- asks env'urlNum
    performEvent_ $ (writeExternalRef numRef) <$> numE

  getRequiredUrlNum reqE = do
    numRef <- asks env'urlNum
    performEvent $ (readExternalRef numRef) <$ reqE

  getUrlList reqE = do
    urlsRef <- asks env'urls
    performEvent $ ffor reqE $ const $ liftIO $ fmap S.elems $ readExternalRef urlsRef

  addUrls urlsE = do
    urlsRef <- asks env'urls
    performEvent_ $ ffor urlsE $ \urls ->
      modifyExternalRef urlsRef (\s -> (S.union (S.fromList urls) s, ()) )

  invalidateUrls urlsE = do
    urlsRef <- asks env'urls
    performEvent_ $ ffor urlsE $ \urls ->
      modifyExternalRef urlsRef (\s -> (S.difference s (S.fromList urls), ()) )
  getUrlsRef = asks env'urls
  getRequiredUrlNumRef = asks env'urlNum
  getRequestTimeoutRef = asks env'timeout
