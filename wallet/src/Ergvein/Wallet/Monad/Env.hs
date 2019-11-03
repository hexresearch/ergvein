{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Env(
    Env(..)
  , newEnv
  , runEnv
  , liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent.Chan (Chan)
import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor (void)
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Data.Time
import Ergvein.Crypto
import Ergvein.Wallet.Alert.Type
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Monad.Unauth
import Ergvein.Wallet.Native
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Settings (Settings(..))
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Password
import Network.Haskoin.Address
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef


import qualified Data.Map.Strict as M

data Env t = Env {
  env'settings        :: !Settings
, env'backEF          :: !(Event t (), IO ())
, env'loading         :: !(Event t (Text, Bool), (Text, Bool) -> IO ())
, env'langRef         :: !(ExternalRef t Language)
, env'authRef         :: (ExternalRef t AuthInfo)     -- Non strict so that undefined from newEnv does not cause panic. Will initialize later
, env'storeDir        :: !Text
, env'alertsEF        :: (Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alert event and trigger
, env'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, env'logsNameSpaces  :: !(ExternalRef t [Text])
, env'uiChan          :: !(Chan (IO ()))
}

type ErgveinM t m = ReaderT (Env t) m

newEnv :: (Reflex t, TriggerEvent t m, MonadIO m)
  => Settings
  -> Chan (IO ()) -- UI callbacks channel
  -> m (Env t)
newEnv settings uiChan = do
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  alertsEF <- newTriggerEvent
  authEF <- newTriggerEvent
  langRef <- newExternalRef $ settingsLang settings
  re <- newRetractEnv
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []
  pure Env {
      env'settings        = settings
    , env'backEF          = (backE, backFire ())
    , env'loading         = loadingEF
    , env'langRef         = langRef
    , env'storeDir        = settingsStoreDir settings
    , env'authRef      = undefined
    , env'alertsEF        = alertsEF
    , env'logsTrigger     = logsTrigger
    , env'logsNameSpaces  = nameSpaces
    , env'uiChan          = uiChan
    }

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir

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
  getSettings = asks env'settings
  {-# INLINE getSettings #-}
  getLoadingWidgetTF = asks env'loading
  {-# INLINE getLoadingWidgetTF #-}
  toggleLoadingWidget reqE = do
    fire <- asks (snd . env'loading)
    performEvent_ $ (liftIO . fire) <$> reqE
  {-# INLINE toggleLoadingWidget #-}
  loadingWidgetDyn reqD = do
    fire <- asks (snd . env'loading)
    performEvent_ $ (liftIO . fire) <$> (updated reqD)
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
  getAuthInfoMaybe = (fmap . fmap) Just $ externalRefDynamic =<< asks env'authRef
  {-# INLINE getAuthInfoMaybe #-}
  setAuthInfo e = do
    authRef <- asks env'authRef
    performEvent $ ffor e $ \case
      Nothing -> pure ()
      Just v -> writeExternalRef authRef v
  {-# INLINE setAuthInfo #-}

instance MonadBaseConstr t m => MonadAlertPoster t (ErgveinM t m) where
  postAlert e = do
    (_, fire) <- asks env'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . env'alertsEF)
  getAlertEventFire = asks env'alertsEF
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance MonadBaseConstr t m => MonadStorage t (ErgveinM t m) where
  getEncryptedWallet = fmap storage'wallet $ readExternalRef =<< asks env'authRef
  {-# INLINE getEncryptedWallet #-}
  getAddressesByEgvXPubKey k = do
    let net = getNetworkFromTag $ egvXPubNetTag k
    keyMap <- fmap storage'pubKeys $ readExternalRef =<< asks env'authRef
    pure $ catMaybes $ maybe [] (fmap (stringToAddr net)) $ M.lookup k keyMap
  {-# INLINE getAddressesByEgvXPubKey #-}

runEnv :: (MonadBaseConstr t m, PlatformNatives)
  => RunCallbacks -> Env t -> ReaderT (Env t) (RetractT t m) a -> m a
runEnv cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ (snd . env'backEF) e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = (void $ retract . fst =<< getBackEventFire) >> ma

type Password = Text

-- | Execute action under authorized context or return the given value as result
-- is user is not authorized. Each time the login info changes (user logs out or logs in)
-- the widget is updated.
liftAuth :: MonadFrontBase t m => m a -> (ErgveinM t m) a -> m (Dynamic t a)
liftAuth ma0 ma = mdo
  mauthD <- holdUniqDyn =<< getAuthInfoMaybe
  mauth0 <- sample . current $ mauthD
  let runAuthed auth = do
        settings        <- getSettings
        backEF          <- getBackEventFire
        loading         <- getLoadingWidgetTF
        langRef         <- getLangRef
        authRef         <- newExternalRef auth
        storeDir        <- getStoreDir
        alertsEF        <- getAlertEventFire
        logsTrigger     <- getLogsTrigger
        logsNameSpaces  <- getLogsNameSpacesRef
        uiChan          <- getUiChan
        let infoE = externalEvent authRef
        a <- runReaderT ma $ Env
          settings backEF loading langRef authRef storeDir alertsEF
          logsTrigger logsNameSpaces uiChan
        pure (a, infoE)
  let
    ma0e = (,never) <$> ma0
    ma0' = maybe ma0e runAuthed mauth0
    redrawE = updated mauthD
  dres :: Dynamic t (a, Event t AuthInfo) <- widgetHold ma0' $ ffor redrawE $ maybe ma0e runAuthed
  let authInfoE = switch . current . fmap snd $ dres
  _ <- setAuthInfo $ Just <$> authInfoE
  pure $ fst <$> dres

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma
