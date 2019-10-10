-- | Pretty printing of values with localization
module Reflex.Localize.Class(
    GrammarCase(..)
  , LocalizedPrint(..)
  , defaultLocPrintDyn
  , localizedText
  , localizedTextLower
  , localizedTextUpper
) where

import Data.Data
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import Reflex
import Reflex.Dom
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
class LocalizedPrint l a where
  -- | Convert value to localized string
  localizedShow :: l -> a -> Text
  localizedShow = localizedShowCased Nominative
  {-# INLINE localizedShow #-}

  -- | Convert value to localized string using grammar case
  localizedShowCased :: GrammarCase -> l -> a -> Text
  localizedShowCased _ = localizedShow
  {-# INLINE localizedShowCased #-}

  -- | Convert value to a dynamically changed string
  localized :: MonadLocalized t m l => a -> m (Dynamic t Text)
  localized = defaultLocPrintDyn localizedShow
  {-# INLINE localized #-}

instance LocalizedPrint l Text where
  localizedShow _ = id
  {-# INLINE localizedShow #-}
  localized = pure . pure
  {-# INLINE localized #-}

instance (LocalizedPrint l a, LocalizedPrint l b) => LocalizedPrint l (Either a b) where
  localizedShow l = either (localizedShow l) (localizedShow l)
  {-# INLINE localizedShow #-}
  localized = either localized localized
  {-# INLINE localized #-}

-- | Default implementation
defaultLocPrintDyn :: MonadLocalized t m l => (l -> a -> Text) -> a -> m (Dynamic t Text)
defaultLocPrintDyn f v = fmap (fmap (flip f v)) getLanguage

localizedText :: (MonadLocalized t m l, LocalizedPrint l a) => a -> m ()
localizedText val = dynText =<< localized val

localizedTextLower :: (MonadLocalized t m l, LocalizedPrint l a) => a -> m ()
localizedTextLower val = dynText =<< (fmap2 T.toLower $ localized val)
  where
    fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
    fmap2 f x = fmap (fmap f) x

localizedTextUpper :: (MonadLocalized t m l, LocalizedPrint l a) => a -> m ()
localizedTextUpper val = dynText =<< (fmap2 T.toUpper $ localized val)
  where
    fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
    fmap2 f x = fmap (fmap f) x
