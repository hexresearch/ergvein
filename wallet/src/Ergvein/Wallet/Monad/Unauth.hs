module Ergvein.Wallet.Monad.Unauth
  (
    UnauthEnv(..)
  , newUnauthEnv
  , runUnauth
  ) where

import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.IORef
import Data.Text (Text)
import Ergvein.Wallet.Language
import Ergvein.Wallet.Log.Types
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Settings
import Reflex
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize

data UnauthEnv t = UnauthEnv {
  unauth'settings        :: !Settings
, unauth'backEF          :: !(Event t (), IO ())
, unauth'loading         :: !(Event t (Text, Bool), (Text, Bool) -> IO ())
, unauth'langRef         :: !(ExternalRef t Language)
, unauth'storeDir        :: !Text
, unauth'errorsEF        :: (Event t ErrorInfo, ErrorInfo -> IO ()) -- ^ Holds errors for error poster
, unauth'logsTrigger     :: (Event t LogEntry, LogEntry -> IO ())
, unauth'logsNameSpaces  :: ExternalRef t [Text]
, unauth'uiChan          :: Chan (IO ())
}

type UnauthM t m = ReaderT (UnauthEnv t) m

instance Monad m => HasStoreDir (UnauthM t m) where
  getStoreDir = asks unauth'storeDir

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

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives) => MonadFrontBase t (UnauthM t m) where
  getSettings = asks unauth'settings
  {-# INLINE getSettings #-}
  getLoadingWidgetTF = asks unauth'loading
  {-# INLINE getLoadingWidgetTF #-}
  toggleLoadingWidget reqE = do
    fire <- asks (snd . unauth'loading)
    performEvent_ $ (liftIO . fire) <$> reqE
  {-# INLINE toggleLoadingWidget #-}
  loadingWidgetDyn reqD = do
    fire <- asks (snd . unauth'loading)
    performEvent_ $ (liftIO . fire) <$> (updated reqD)
  {-# INLINE loadingWidgetDyn #-}
  getBackEventFire = asks unauth'backEF
  {-# INLINE getBackEventFire #-}
  getUiChan = asks unauth'uiChan
  {-# INLINE getUiChan #-}

instance MonadBaseConstr t m => MonadErrorPoster t (UnauthM t m) where
  postError e = do
    (_, fire) <- asks unauth'errorsEF
    performEvent_ $ liftIO . fire <$> e
  newErrorEvent = asks (fst . unauth'errorsEF)
  getErrorEventFire = asks unauth'errorsEF
  {-# INLINE postError #-}
  {-# INLINE newErrorEvent #-}
  {-# INLINE getErrorEventFire #-}
  
newUnauthEnv :: (Reflex t, TriggerEvent t m, MonadIO m)
  => Settings
  -> Chan (IO ()) -- UI callbacks channel
  -> m (UnauthEnv t)
newUnauthEnv settings uiChan = do
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  errorsEF <- newTriggerEvent
  langRef <- newExternalRef $ settingsLang settings
  re <- newRetractEnv
  logsTrigger <- newTriggerEvent
  nameSpaces <- newExternalRef []
  pure UnauthEnv {
      unauth'settings  = settings
    , unauth'backEF    = (backE, backFire ())
    , unauth'loading   = loadingEF
    , unauth'langRef   = langRef
    , unauth'storeDir  = settingsStoreDir settings
    , unauth'errorsEF  = errorsEF
    , unauth'logsTrigger = logsTrigger
    , unauth'logsNameSpaces = nameSpaces
    , unauth'uiChan = uiChan
    }

runUnauth :: (MonadBaseConstr t m, PlatformNatives)
  => RunCallbacks -> UnauthEnv t -> ReaderT (UnauthEnv t) (RetractT t m) a -> m a
runUnauth cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ (snd . unauth'backEF) e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = (void $ retract . fst =<< getBackEventFire) >> ma
