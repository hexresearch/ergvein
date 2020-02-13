module Ergvein.Interfaces.Ergo.Modifiers.History.ValidateChain where

import Data.List
import Data.Ord

import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Scorex.Util.Package


class HasNormaliseHeader a where
  normalizeHeader :: a -> PoPowNormHeader


data PoPowNormHeader = PoPowNormHeader {
  blockId     :: ModifierId
, interlinks' :: [ModifierId]
, level       :: Int
, header'     :: Header
} deriving (Eq)

isValidChainAnchoredTo' :: PoPowNormHeader -> [PoPowNormHeader] -> Bool
isValidChainAnchoredTo' g hs =
  (blockId <$> hs) == (blockId <$> hs')
  where
    hs' = sortOn (Down . height . header') hs
