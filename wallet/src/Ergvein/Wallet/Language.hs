-- | Module that fullfils data family required by `reflex-localize`
module Ergvein.Wallet.Language(
    Language(..)
  ) where

import GHC.Generics (Generic)
import Reflex.Localize.Language
import Ergvein.Aeson

-- | Languages that are supported by wallet
data instance Language
  = English
  | Russian
  deriving (Eq, Ord, Show, Read, Generic)

$(deriveJSON aesonOptions 'English)
