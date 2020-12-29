-- | Pretty printing of values with localization
module Reflex.Localize.Class(
    GrammarCase(..)
  , LocalizedPrint(..)
  , defaultLocPrintDyn
) where

import Data.Data
import Data.Text (Text)
import GHC.Generics
import Reflex
import Reflex.Localize.Language
import Reflex.Localize.Monad

-- | Grammar case to change form of word
data GrammarCase =
    Nominative -- ^ the “subject” case
  | Accusative -- ^  the “direct object” case
  | Genitive -- ^ corresponding to the possessive case or “of + (noun)”
  | Dative  -- ^ corresponding to “to + (noun)" or the indirect object
  | Instrumental -- ^ denoting an instrument used in an action
  | Prepositional -- ^ used with many common prepositions, such as “in”, “on” etc.
  deriving (Eq, Ord, Enum, Show, Read, Bounded, Generic, Data)

-- | Printing human readable localized values.
--
-- Minimal implementation is either 'localizedShow' or 'localizedShowCased'
class LocalizedPrint a where
  -- | Convert value to localized string
  localizedShow :: Language -> a -> Text
  localizedShow = localizedShowCased Nominative
  {-# INLINE localizedShow #-}

  -- | Convert value to localized string using grammar case
  localizedShowCased :: GrammarCase -> Language -> a -> Text
  localizedShowCased _ = localizedShow
  {-# INLINE localizedShowCased #-}

  -- | Convert value to a dynamically changed string
  localized :: MonadLocalized t m => a -> m (Dynamic t Text)
  localized = defaultLocPrintDyn localizedShow
  {-# INLINE localized #-}

instance LocalizedPrint Text where
  localizedShow _ = id
  {-# INLINE localizedShow #-}
  localized = pure . pure
  {-# INLINE localized #-}

instance (LocalizedPrint a, LocalizedPrint b) => LocalizedPrint (Either a b) where
  localizedShow l = either (localizedShow l) (localizedShow l)
  {-# INLINE localizedShow #-}
  localized = either localized localized
  {-# INLINE localized #-}

-- | Default implementation
defaultLocPrintDyn :: MonadLocalized t m => (Language -> a -> Text) -> a -> m (Dynamic t Text)
defaultLocPrintDyn f v = fmap (fmap (flip f v)) getLanguage
