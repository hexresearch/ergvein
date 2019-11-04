-- | Module that fullfils data family required by `reflex-localize`
module Ergvein.Wallet.Language(
    Language(..)
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
  deriving (Eq, Ord, Show, Read, Generic)

$(deriveJSON aesonOptions 'English)
