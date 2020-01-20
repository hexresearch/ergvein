module Ergvein.Interfaces.Ergo.Modifiers.History.PoPowProofPrefix where

import Data.Aeson as A
import Data.Int

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Modifiers.History.PoPow
import Ergvein.Interfaces.Ergo.PoPowHeader
import Ergvein.Interfaces.Ergo.Scorex.Util.Package

data PoPowProofPrefix = PoPowProofPrefix {
  prefixM  :: !Int32
, chain    :: ![PoPowHeader]
, suffixId :: !ModifierId
, size     :: !(Maybe Int32)
} deriving (Eq, Show)

instance IsChain [PoPowHeader] where
  type Element [PoPowHeader] = PoPowHeader
  type Container [PoPowHeader] = []
  chainElems = id
  chainLength = length
  chainFromList = id
  findDivergingSubchains = findDivergingSubchainsWithList
  isValidChainAnchoredTo = undefined


deriveJSON (A.defaultOptions { fieldLabelModifier = (\case { "prefixM" -> "m"; a -> a; }) }) ''PoPowProofPrefix
