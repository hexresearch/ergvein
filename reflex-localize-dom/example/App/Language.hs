module App.Language(
    Language(..)
  , module Reflex.Localize
  ) where

import Reflex.Localize
import Reflex.Localize.Language

data instance Language
  = English
  | Russian
  deriving (Eq, Ord, Enum, Bounded)

instance LocalizedPrint Language where
  localizedShow l v = case l of
    English -> case v of
      English -> "English"
      Russian -> "Russian"
    Russian -> case v of
      English -> "Английский"
      Russian -> "Русский"
