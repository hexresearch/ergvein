{-# LANGUAGE UndecidableInstances #-}
module Ergvein.Wallet.Monad.Env(
    Env(..)
  , newEnv
  , runEnv
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Data.Functor (void)
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Network.Haskoin.Address
import Reflex
import Reflex.Dom
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize

import qualified Data.Map.Strict as M

data Env t = Env {
  env'settings  :: !Settings
, env'backEvent :: !(Event t ())
, env'backFire  :: !(IO ())
, env'loading   :: !(Event t (Text, Bool), (Text, Bool) -> IO ())
, env'langRef   :: !(ExternalRef t Language)
, env'storage   :: ErgveinStorage     -- Non strict so that undefined from newEnv does not cause panic. Will initialize later
, env'storeDir  :: !Text
, env'errorsEF  :: (Event t ErrorInfo, ErrorInfo -> IO ()) -- ^ Holds errors for error poster
}

type ErgveinM t m = ReaderT (Env t) m

newEnv :: (Reflex t, TriggerEvent t m, MonadIO m) => Settings -> m (Env t)
newEnv settings = do
  (backE, backFire) <- newTriggerEvent
  loadingEF <- newTriggerEvent
  errorsEF <- newTriggerEvent
  langRef <- newExternalRef $ settingsLang settings
  re <- newRetractEnv
  pure Env {
      env'settings  = settings
    , env'backEvent = backE
    , env'backFire  = backFire ()
    , env'loading   = loadingEF
    , env'langRef   = langRef
    , env'storeDir  = settingsStoreDir settings
    , env'storage   = undefined
    , env'errorsEF  = errorsEF
    }

instance Monad m => HasStoreDir (ErgveinM t m) where
  getStoreDir = asks env'storeDir

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

instance (MonadBaseConstr t m) => MonadBackable t (ErgveinM t m) where
  getBackEvent = asks env'backEvent
  {-# INLINE getBackEvent #-}

instance (MonadBaseConstr t m, MonadRetract t m, PlatformNatives) => MonadFront t (ErgveinM t m) where
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

instance MonadBaseConstr t m => MonadErrorPoster t (ErgveinM t m) where
  postError e = do
    (_, fire) <- asks env'errorsEF
    performEvent_ $ liftIO . fire <$> e
  newErrorEvent = asks (fst . env'errorsEF)
  {-# INLINE postError #-}
  {-# INLINE newErrorEvent #-}

instance MonadBaseConstr t m => MonadStorage t (ErgveinM t m) where
  getEncryptedWallet = asks (storageWallet . env'storage)
  getAddressesByEgvXPubKey k = do
    let net = getNetworkFromTag $ egvXPubNetTag k
    keyMap <- asks (storagePubKeys . env'storage)
    pure $ catMaybes $ maybe [] (fmap (stringToAddr net)) $ M.lookup k keyMap

runEnv :: MonadBaseConstr t m
  => RunCallbacks -> Env t -> ReaderT (Env t) (RetractT t m) a -> m a
runEnv cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ env'backFire e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = (void $ retract =<< getBackEvent) >> ma
