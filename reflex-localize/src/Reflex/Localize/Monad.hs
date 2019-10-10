module Reflex.Localize.Monad
  (
    MonadLocalized(..)
  ) where

import Control.Monad.Reader

import Reflex
import Reflex.Dom

-- | ===========================================================================
-- |                        Monad Localized
-- | ===========================================================================

-- | API for language localization support
class (Reflex t, Monad m, PostBuild t m, DomBuilder t m) => MonadLocalized t m l | m -> t, m -> l where
  -- | Switch frontend language
  setLanguage :: l -> m ()
  -- | Get language of the frontend
  getLanguage :: m (Dynamic t l)

instance {-# OVERLAPPABLE #-} MonadLocalized t m l => MonadLocalized t (ReaderT e m) l where
  setLanguage = lift . setLanguage
  getLanguage = lift getLanguage
  {-# INLINE setLanguage #-}
  {-# INLINE getLanguage #-}
