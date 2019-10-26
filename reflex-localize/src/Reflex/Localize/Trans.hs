-- |
-- Module      : Reflex.Localize.Trans
-- Copyright   : (c) 2019 ATUM SOLUTIONS AG
-- License     : MIT
-- Maintainer  : lemarwin42@gmail.com
-- Stability   : unstable
-- Portability : non-portable
--
-- Plug-in implementation for `MonadLocalized` using wrapper around `ReaderT`.
-- Internal module, implementation details can be changed at any moment.
module Reflex.Localize.Trans where

import Control.Monad.Reader
import Control.Monad.State.Strict
import GHC.Generics
import Language.Javascript.JSaddle.Types
import Reflex
import Reflex.Dom
import Reflex.ExternalRef
import Reflex.Localize.Language
import Reflex.Localize.Monad

data LocalizeEnv t = LocalizeEnv {
  locEnvLangRef :: !(ExternalRef t Language)
} deriving (Generic)

-- | Allocate new environment for `LocalizeT`.
newLangEnv :: (Reflex t, TriggerEvent t m, MonadIO m) => Language -> m (LocalizeEnv t)
newLangEnv initLang = fmap LocalizeEnv $ newExternalRef initLang

-- | Plug-in implementation of `MonadLocalized`.
newtype LocalizeT t m a = LocalizeT { unLocalizeT :: ReaderT (LocalizeEnv t) m a }
  deriving (Functor, Applicative, Monad, Generic, MonadFix)

deriving instance PostBuild t m => PostBuild t (LocalizeT t m)
deriving instance NotReady t m => NotReady t (LocalizeT t m)
deriving instance PerformEvent t m => PerformEvent t (LocalizeT t m)
deriving instance TriggerEvent t m => TriggerEvent t (LocalizeT t m)
deriving instance MonadHold t m => MonadHold t (LocalizeT t m)
deriving instance MonadSample t m => MonadSample t (LocalizeT t m)
deriving instance DomBuilder t m => DomBuilder t (LocalizeT t m)
deriving instance MonadIO m => MonadIO (LocalizeT t m)
deriving instance MonadJSM m => MonadJSM (LocalizeT t m)
deriving instance (Group q, Additive q, Query q, Eq q, MonadQuery t q m, Monad m) => MonadQuery t q (LocalizeT t m)
deriving instance (Monoid w, DynamicWriter t w m) => DynamicWriter t w (LocalizeT t m)
deriving instance (Monoid w, MonadBehaviorWriter t w m) => MonadBehaviorWriter t w (LocalizeT t m)
deriving instance (Semigroup w, EventWriter t w m) => EventWriter t w (LocalizeT t m)
deriving instance (Requester t m) => Requester t (LocalizeT t m)

instance MonadTrans (LocalizeT t) where
  lift = LocalizeT . lift
  {-# INLINABLE lift #-}

instance MonadReader e m => MonadReader e (LocalizeT t m) where
  ask = lift ask
  {-# INLINABLE ask #-}
  local f (LocalizeT ma) = LocalizeT $ do
    r <- ask
    lift $ local f $ runReaderT ma r
  {-# INLINABLE local #-}

instance MonadState s m => MonadState s (LocalizeT t m) where
  get = lift get
  {-# INLINABLE get #-}
  put = lift . put
  {-# INLINABLE put #-}

instance Adjustable t m => Adjustable t (LocalizeT t m) where
  runWithReplace a0 a' = do
    r <- LocalizeT ask
    lift $ runWithReplace (runLocalizeT a0 r) $ fmap (`runLocalizeT` r) a'
  {-# INLINABLE runWithReplace #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- LocalizeT ask
    lift $ traverseIntMapWithKeyWithAdjust (\k v -> runLocalizeT (f k v) r) dm0 dm'
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- LocalizeT ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runLocalizeT (f k v) r) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- LocalizeT ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runLocalizeT (f k v) r) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjustWithMove #-}

-- | Execute localization widget with given environment.
runLocalizeT :: LocalizeT t m a -> LocalizeEnv t -> m a
runLocalizeT (LocalizeT ma) e = runReaderT ma e
{-# INLINEABLE runLocalizeT #-}

-- | Simplified version of `runLocalizeT`
runLocalize :: (Reflex t, TriggerEvent t m, MonadIO m) => Language -> LocalizeT t m a -> m a
runLocalize initLang ma = do
  re <- newLangEnv initLang
  runLocalizeT ma re
{-# INLINABLE runLocalize #-}

instance (PerformEvent t m, MonadHold t m, Adjustable t m, MonadFix m, MonadIO (Performable m), PostBuild t m, MonadIO m, DomBuilder t m)
  => MonadLocalized t (LocalizeT t m) where
  setLanguage lang = do
    langRef <- LocalizeT $ asks locEnvLangRef
    writeExternalRef langRef lang
  getLanguage = LocalizeT $ (externalRefDynamic =<< asks locEnvLangRef)
  {-# INLINE setLanguage #-}
  {-# INLINE getLanguage #-}
