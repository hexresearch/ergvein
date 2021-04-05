module Sepulcas.Monad.Env(
    Sepulca(..)
  , SepulcaM
  , newSepulca
  , runSepulca
  ) where

import Control.Concurrent.Chan
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import Reflex
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize
import Reflex.Localize.Language
import Sepulcas.Alert.Types
import Sepulcas.Log
import Sepulcas.Monad.Class
import Sepulcas.Monad.Password
import Sepulcas.Run.Callbacks

data Sepulca t = Sepulca {
  sepulca'storeDir        :: !Text
, sepulca'pauseEF         :: !(Event t (), IO ())
, sepulca'resumeEF        :: !(Event t (), IO ())
, sepulca'backEF          :: !(Event t (), IO ())
, sepulca'loading         :: !(Event t (Bool, Text), (Bool, Text) -> IO ())
, sepulca'uiChan          :: !(Chan (IO ()))
, sepulca'logsTrigger     :: !(Event t LogEntry, LogEntry -> IO ())
, sepulca'logsNameSpaces  :: !(ExternalRef t [Text])
, sepulca'langRef         :: !(ExternalRef t Language)
, sepulca'alertsEF        :: !(Event t AlertInfo, AlertInfo -> IO ())
, sepulca'passModalEF     :: !(Event t (Int, Text), (Int, Text) -> IO ())
, sepulca'passSetEF       :: !(Event t (Int, Maybe Password), (Int, Maybe Password) -> IO ())
}

type SepulcaM t m = ReaderT (Sepulca t) m

instance Monad m => HasStoreDir (SepulcaM t m) where
  getStoreDir = asks sepulca'storeDir
  {-# INLINE getStoreDir #-}

instance MonadIO m => MonadHasUI (SepulcaM t m) where
  getUiChan = asks sepulca'uiChan
  {-# INLINE getUiChan #-}

instance MonadReflex t m => MonadNativeLogger t (SepulcaM t m) where
  getLogsTrigger = asks sepulca'logsTrigger
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = asks sepulca'logsNameSpaces
  {-# INLINE getLogsNameSpacesRef #-}

instance MonadIO m => HasPassModal t (SepulcaM t m) where
  getPasswordModalEF = asks sepulca'passModalEF
  {-# INLINE getPasswordModalEF #-}
  getPasswordSetEF = asks sepulca'passSetEF
  {-# INLINE getPasswordSetEF #-}

instance MonadReflex t m => MonadLocalized t (SepulcaM t m) where
  setLanguage lang = do
    langRef <- asks sepulca'langRef
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks sepulca'langRef
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks sepulca'langRef
  {-# INLINE getLanguage #-}

instance (MonadReflex t m, MonadRetract t m, PlatformNatives) => Sepulcable t (SepulcaM t m) where
  getLoadingWidgetTF = asks sepulca'loading
  {-# INLINE getLoadingWidgetTF #-}
  getPauseEventFire = asks sepulca'pauseEF
  {-# INLINE getPauseEventFire #-}
  getResumeEventFire = asks sepulca'resumeEF
  {-# INLINE getResumeEventFire #-}
  getBackEventFire = asks sepulca'backEF
  {-# INLINE getBackEventFire #-}
  getLangRef = asks sepulca'langRef
  {-# INLINE getLangRef #-}

instance MonadReflex t m => MonadAlertPoster t (SepulcaM t m) where
  postAlert e = do
    (_, fire) <- asks sepulca'alertsEF
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . sepulca'alertsEF)
  getAlertEventFire = asks sepulca'alertsEF
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

newSepulca :: (MonadReflex t m, PlatformNatives)
  => Maybe Text -- ^ Storage directory
  -> Language -- ^ Default language
  -> Chan (IO ()) -- ^ UI callbacks channel
  -> m (Sepulca t)
newSepulca mstoreDir defLang uiChan = do
  (pauseE, pauseFire) <- newTriggerEvent
  (resumeE, resumeFire) <- newTriggerEvent
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  alertsEF <- newTriggerEvent
  langRef <- newExternalRef defLang
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []
  storeDir <- maybe getHomeDir pure mstoreDir
  passSetEF <- newTriggerEvent
  passModalEF <- newTriggerEvent
  let env = Sepulca {
          sepulca'storeDir        = storeDir
        , sepulca'pauseEF         = (pauseE, pauseFire ())
        , sepulca'resumeEF        = (resumeE, resumeFire ())
        , sepulca'backEF          = (backE, backFire ())
        , sepulca'loading         = loadingEF
        , sepulca'langRef         = langRef
        , sepulca'alertsEF        = alertsEF
        , sepulca'logsTrigger     = logsTrigger
        , sepulca'logsNameSpaces  = nameSpaces
        , sepulca'uiChan          = uiChan
        , sepulca'passModalEF     = passModalEF
        , sepulca'passSetEF       = passSetEF
        }
  pure env

runSepulca :: (MonadReflex t m, PlatformNatives)
  => RunCallbacks -> Sepulca t -> ReaderT (Sepulca t) (RetractT t m) a -> m a
runSepulca cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ (snd . sepulca'backEF) e
  liftIO $ writeIORef (runPauseCallback cbs) $ (snd . sepulca'pauseEF) e
  liftIO $ writeIORef (runResumeCallback cbs) $ (snd . sepulca'resumeEF) e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = void (retract . fst =<< getBackEventFire) >> ma
