module Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProof where

import Data.Aeson as A

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofPrefix
import Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofSuffix

data PoPowProof = PoPowProof {
  prefix :: !PoPowProofPrefix
, suffix :: !PoPowProofSuffix
} deriving (Eq, Show)

deriveJSON A.defaultOptions ''PoPowProof
