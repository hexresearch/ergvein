module Ergvein.Wallet.Version(
    Version(..)
  , embedVersion
  , makeVersionString
  , HasVersion(..)
) where

import Data.Text    (Text)

import Ergvein.Wallet.Version.Internal

-- | Decouple version usage from embedding place. The class should be implemented
-- in executable package to prevent uneeded recompilation of library part.
class HasVersion where
  version :: Version
