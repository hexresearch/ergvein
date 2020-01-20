module Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProof where

import Data.Aeson as A
import Data.Int

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Modifiers.History.PoPow
import Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofPrefix
import Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofSuffix
import Ergvein.Interfaces.Ergo.PoPowHeader

data PoPowProof = PoPowProof {
  prefix :: !PoPowProofPrefix
, suffix :: !PoPowProofSuffix
} deriving (Eq, Show)

poPowProofM :: PoPowProof -> Int32
poPowProofM = prefixM . prefix

poPowProofK :: PoPowProof -> Int32
poPowProofK = suffixK . suffix

instance (p ~ ([PoPowHeader], [PoPowHeader])) => Proof p where
  type Chain p = [PoPowHeader]
  proofPrefix = fst
  proofSuffix = snd
  mkProof = (,)

deriveJSON A.defaultOptions ''PoPowProof
