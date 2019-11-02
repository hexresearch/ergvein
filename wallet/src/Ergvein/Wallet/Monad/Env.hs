{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Env(
    Env(..)
  , newEnv
  , runEnv
  , liftEnv
  , requestAuth
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
, env'storage         :: ErgveinStorage     -- Non strict so that undefined from newEnv does not cause panic. Will initialize later
, env'storeDir        :: !Text
, env'alertsEF        :: (Event t AlertInfo, AlertInfo -> IO ()) -- ^ Holds alert event and trigger
, env'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, env'logsNameSpaces  :: ExternalRef t [Text]
, env'uiChan          :: Chan (IO ())
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
  langRef <- newExternalRef $ settingsLang settings
  re <- newRetractEnv
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []
  pure Env {
      env'settings  = settings
    , env'backEF    = (backE, backFire ())
    , env'loading   = loadingEF
    , env'langRef   = langRef
    , env'storeDir  = settingsStoreDir settings
    , env'storage   = undefined
    , env'alertsEF  = alertsEF
    , env'logsTrigger = logsTrigger
    , env'logsNameSpaces = nameSpaces
    , env'uiChan = uiChan
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
  getEncryptedWallet = asks (storageWallet . env'storage)
  {-# INLINE getEncryptedWallet #-}
  getAddressesByEgvXPubKey k = do
    let net = getNetworkFromTag $ egvXPubNetTag k
    keyMap <- asks (storagePubKeys . env'storage)
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

requestAuth :: MonadFrontBase t m => m a -> (ErgveinM t m) a -> m (Dynamic t a)
requestAuth m0 ma = mdo
  drawE <- fmap (True <$) getPostBuild
  passE <- fmap (switch . current) $ widgetHold (pure never) $ ffor (leftmost [drawE, hideE]) $
    \b -> if b then askPassword else pure never
  hideE <- fmap (False <$) $ delay 0.01 passE
  envE  <- liftEnv passE
  widgetHold m0 $ (runReaderT ma) <$> envE

liftEnv :: (MonadFrontBase t m) => Event t Password -> m (Event t (Env t))
liftEnv passE = do
  storeDir <- getStoreDir
  estorageE <- performEvent $ ffor passE $ \pass -> do
    ts <- liftIO $ getCurrentTime
    estore <- flip runReaderT storeDir $ loadStorageFromFile pass
    pure (ts,estore)
  postAlert $ ffor estorageE $ \(ts, estore) -> case estore of
    Left err -> AlertInfo AlertTypeFail 10 ["Storage"] ts err
    Right _  -> AlertInfo AlertTypeSuccess 10 ["Storage"] ts SALoadedSucc
  settings        <- getSettings
  backEF          <- getBackEventFire
  loading         <- getLoadingWidgetTF
  alertsEF        <- getAlertEventFire
  logsTrigger     <- getLogsTrigger
  logsNameSpaces  <- getLogsNameSpacesRef
  uiChan          <- getUiChan
  langRef         <- getLangRef
  pure $ fforMaybe estorageE $ \(_, estore) -> case estore of
    Left _ -> Nothing
    Right store -> Just $ Env {
          env'settings        = settings
        , env'backEF          = backEF
        , env'loading         = loading
        , env'langRef         = langRef
        , env'storeDir        = storeDir
        , env'storage         = store
        , env'alertsEF        = alertsEF
        , env'logsTrigger     = logsTrigger
        , env'logsNameSpaces  = logsNameSpaces
        , env'uiChan          = uiChan
        }
