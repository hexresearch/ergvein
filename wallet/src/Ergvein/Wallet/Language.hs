-- | Module that fullfils data family required by `reflex-localize`
module Ergvein.Wallet.Language(
    Language(..)
  , allLanguages
  , module Reflex.Localize
  ) where

import Ergvein.Aeson
import GHC.Generics (Generic)
import Reflex.Localize
import Reflex.Localize.Language

-- | Languages that are supported by wallet
data instance Language
  = English
  | Russian
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance LocalizedPrint Language where
  localizedShow _ v = case v of
    English -> "English"
    Russian -> "Русский"

$(deriveJSON aesonOptions 'English)

allLanguages :: [Language]
allLanguages = [minBound .. maxBound]
