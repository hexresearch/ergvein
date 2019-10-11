module Reflex.Localize.Monad
  (
    MonadLocalized(..)
  ) where

import Control.Monad.Reader
import Reflex
import Reflex.Dom
import Reflex.Localize.Language 

-- | ===========================================================================
-- |                        Monad Localized
-- | ===========================================================================

-- | API for language localization support
class (Reflex t, Monad m, PostBuild t m, DomBuilder t m) => MonadLocalized t m | m -> t where
  -- | Switch frontend language
  setLanguage :: Language -> m ()
  -- | Get language of the frontend
  getLanguage :: m (Dynamic t Language)

instance {-# OVERLAPPABLE #-} MonadLocalized t m => MonadLocalized t (ReaderT e m) where
  setLanguage = lift . setLanguage
  getLanguage = lift getLanguage
  {-# INLINE setLanguage #-}
  {-# INLINE getLanguage #-}
