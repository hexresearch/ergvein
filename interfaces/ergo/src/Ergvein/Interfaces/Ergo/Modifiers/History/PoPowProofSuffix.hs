module Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofSuffix where

import Data.Aeson as A
import Data.Int

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.PoPowHeader

data PoPowProofSuffix = PoPowProofSuffix {
  suffixK :: !Int32
, chain   :: ![PoPowHeader]
, sizeOpt :: !(Maybe Int32)
} deriving (Eq, Show)

deriveJSON (A.defaultOptions { fieldLabelModifier = (\case { "suffixK" -> "k"; a -> a; }) }) ''PoPowProofSuffix
