module Ergvein.Wallet.Monad.Unauth
  (
    UnauthEnv(..)
  , newUnauthEnv
  , runUnauth
  ) where

import Control.Monad.Reader
import Data.IORef
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Reflex
import Reflex.Dom.Retractable
import Reflex.ExternalRef
import Reflex.Localize

data UnauthEnv t = UnauthEnv {
  unauthEnv'lang :: !(ExternalRef t Language)
, unauthEnv'backEvent :: !(Event t ())
, unauthEnv'backFire  :: !(IO ())
}

instance MonadBaseConstr t m => MonadLocalized t (ReaderT (UnauthEnv t) m) where
  setLanguage lang = do
    langRef <- asks unauthEnv'lang
    writeExternalRef langRef lang
  {-# INLINE setLanguage #-}
  setLanguageE langE = do
    langRef <- asks unauthEnv'lang
    performEvent_ $ fmap (writeExternalRef langRef) langE
  {-# INLINE setLanguageE #-}
  getLanguage = externalRefDynamic =<< asks unauthEnv'lang
  {-# INLINE getLanguage #-}

instance (MonadBaseConstr t m) => MonadBackable t (ReaderT (UnauthEnv t) m) where
  getBackEvent = asks unauthEnv'backEvent
  {-# INLINE getBackEvent #-}

newUnauthEnv :: (Reflex t, TriggerEvent t m, MonadIO m) => Language -> m (UnauthEnv t)
newUnauthEnv initLang = do
  langRef <- newExternalRef initLang
  (backE, backFire) <- newTriggerEvent
  pure $ UnauthEnv {
      unauthEnv'lang      = langRef
    , unauthEnv'backEvent = backE
    , unauthEnv'backFire  = backFire ()
    }

runUnauth :: MonadBaseConstr t m
  => RunCallbacks -> UnauthEnv t -> ReaderT (UnauthEnv t) (RetractT t m) a -> m a
runUnauth cbs e ma = do
  liftIO $ writeIORef (runBackCallback cbs) $ unauthEnv'backFire e
  re <- newRetractEnv
  runRetractT (runReaderT ma' e) re
  where
    ma' = (void $ retract =<< getBackEvent) >> ma
