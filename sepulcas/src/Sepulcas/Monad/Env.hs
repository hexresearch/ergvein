module Sepulcas.Monad.Env(
    Sepulca(..)
  , HasSepulca(..)
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
}

class Monad m => HasSepulca t m | m -> t where
  getSepulca :: m (Sepulca t)

type SepulcaM t m = ReaderT (Sepulca t) m

instance Monad m => HasSepulca t (SepulcaM t m) where
  getSepulca = ask
  {-# INLINE getSepulca #-}

instance {-# OVERLAPPABLE #-} HasSepulca t m => HasStoreDir m where
  getStoreDir = sepulca'storeDir <$> getSepulca
  {-# INLINE getStoreDir #-}

instance {-# OVERLAPPABLE #-} (HasSepulca t m, MonadIO m) => MonadHasMain m where
  getMainThreadChan = sepulca'uiChan <$> getSepulca
  {-# INLINE getMainThreadChan #-}

instance {-# OVERLAPPABLE #-} (HasSepulca t m, MonadReflex t m) => MonadNativeLogger t m where
  getLogsTrigger = sepulca'logsTrigger  <$> getSepulca
  {-# INLINE getLogsTrigger #-}
  getLogsNameSpacesRef = sepulca'logsNameSpaces <$> getSepulca
  {-# INLINE getLogsNameSpacesRef #-}

instance {-# OVERLAPPABLE #-} (HasSepulca t m, MonadReflex t m, Reflex t) => MonadLocalized t m where
  setLanguage lang = do
    langRef <- sepulca'langRef <$> getSepulca
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- sepulca'langRef <$> getSepulca
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< (sepulca'langRef <$> getSepulca)
  {-# INLINE getLanguage #-}

instance {-# OVERLAPPABLE #-} (HasSepulca t m, MonadReflex t m, MonadRetract t m, PlatformNatives) => Sepulcable t m where
  getLoadingWidgetTF = sepulca'loading <$> getSepulca
  {-# INLINE getLoadingWidgetTF #-}
  getPauseEventFire = sepulca'pauseEF <$> getSepulca
  {-# INLINE getPauseEventFire #-}
  getResumeEventFire = sepulca'resumeEF <$> getSepulca
  {-# INLINE getResumeEventFire #-}
  getBackEventFire = sepulca'backEF <$> getSepulca
  {-# INLINE getBackEventFire #-}
  getLangRef = sepulca'langRef <$> getSepulca
  {-# INLINE getLangRef #-}

instance {-# OVERLAPPABLE #-} (HasSepulca t m, MonadReflex t m) => MonadAlertPoster t m where
  postAlert e = do
    (_, fire) <- sepulca'alertsEF <$> getSepulca
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = (fst . sepulca'alertsEF) <$> getSepulca
  getAlertEventFire = sepulca'alertsEF <$> getSepulca
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
