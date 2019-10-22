-- | Module that fullfils data family required by `reflex-localize`
module Ergvein.Wallet.Language(
    Language(..)
  ) where

import GHC.Generics (Generic)
import Reflex.Localize.Language

-- | Languages that are supported by wallet
data instance Language
  = English
  | Russian
  deriving (Eq, Ord, Show, Read, Generic)
