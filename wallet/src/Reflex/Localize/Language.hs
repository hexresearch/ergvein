-- | Module that fullfils signature required by `reflex-localize`
module Reflex.Localize.Language(
    Language(..)
  ) where

import GHC.Generics (Generic)

-- | Languages that are supported by wallet
data Language
  = English
  | Russian
  deriving (Eq, Ord, Show, Read, Generic)
