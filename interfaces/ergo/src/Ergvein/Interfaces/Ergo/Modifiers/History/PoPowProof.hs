module Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProof where

import Data.Aeson as A
import Data.Int

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofPrefix
import Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofSuffix

data PoPowProof = PoPowProof {
  prefix :: !PoPowProofPrefix
, suffix :: !PoPowProofSuffix
} deriving (Eq, Show)

poPowProofM :: PoPowProof -> Int32
poPowProofM = prefixM . prefix

poPowProofK :: PoPowProof -> Int32
poPowProofK = suffixK . suffix

deriveJSON A.defaultOptions ''PoPowProof
