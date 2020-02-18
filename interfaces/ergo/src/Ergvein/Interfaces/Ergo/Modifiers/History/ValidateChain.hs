module Ergvein.Interfaces.Ergo.Modifiers.History.ValidateChain where

import Control.Arrow
import Control.Monad
import Data.Bool
import Data.List
import Data.Ord
import Data.Profunctor
import Data.String
import Data.Text (Text)
import Safe

import qualified Data.Map.Strict as M

import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Interfaces.Ergo.Scorex.Util.Package


class HasNormaliseHeader a where
  normalizeHeader :: a -> PoPowNormHeader

type PoPowNormHeader = PoPowNormHeaderF ModifierId Header

data PoPowNormHeaderF a h = PoPowNormHeader {
  blockId     :: a
, level       :: Int
, hdr         :: h
, ilinks      :: [a]
} deriving (Eq, Show)

class HasGetHeight a where
  getHeight :: a -> Height

instance HasGetHeight Header where
  getHeight = height

-- | Requirements to chain:
-- 1. Every Block should contain genesis link
-- 2. Interlinks should not come from nowhere. It should either be part of
-- previous block interlinks or be a previous block itself.
isValidChainAnchoredTo' :: (HasGetHeight h, Eq a, b ~ PoPowNormHeaderF a h, Show a) =>
    b -> [b] -> Either Text ()
isValidChainAnchoredTo' g hs = do

    failInCaseOf "Is not sorted by header height" $
        all (uncurry (>)) . uncurry zip . (id &&& (drop 1)) . fmap (getHeight . hdr) $ hs

    failInCaseOf "Is not anchored to genesis" (all isAnchoredToGenesis hs)
    -- First block in the popowproof chain is a special case:
    --   we need (and we can) to check only genesis interlink for the first block.

    forM_ (prepareSubChains level hsr) $ \(d, (h:hs)) -> do
        let
          ilinks' = dimap reverse reverse (drop d) . drop 1 $ ilinks h
          ilinks'' = cookInterlinksFromChain hs
        failInCaseOf ("Interlinks is not valid for block " <> (fromString . show $ blockId h)) $
          all (uncurry (==)) $ zip ilinks' ilinks''

  where
    genesisId = blockId g
    hsr = reverse hs
    isAnchoredToGenesis = (== Just genesisId) . headMay . ilinks

cookInterlinksFromChain = reverse . M.elems
    . foldl' (\c a -> M.union c (M.fromList [(k, blockId a) | k <- [0..level a]])) mempty

-- λ mapM_ print $ prepareSubChains id [4,4,3,3,4,3,2,2,3,1,2,1,0,1,0,0 :: Int]
-- (4,[4,4])
-- (4,[3,4,4])
-- (3,[3,3,4,4])
-- (3,[4,3,3,4,4])
-- (3,[3,4,3,3,4,4])
-- (3,[2,3,4,3,3,4,4])
-- (2,[2,2,3,4,3,3,4,4])
-- (2,[3,2,2,3,4,3,3,4,4])
-- (2,[1,3,2,2,3,4,3,3,4,4])
-- (1,[2,1,3,2,2,3,4,3,3,4,4])
-- (1,[1,2,1,3,2,2,3,4,3,3,4,4])
-- (1,[0,1,2,1,3,2,2,3,4,3,3,4,4])
-- (0,[1,0,1,2,1,3,2,2,3,4,3,3,4,4])
-- (0,[0,1,0,1,2,1,3,2,2,3,4,3,3,4,4])
-- (0,[0,0,1,0,1,2,1,3,2,2,3,4,3,3,4,4])
-- | It calculates subchains along numbers of interlinks that need to be ignored
prepareSubChains :: (Ord b, Bounded b) => (a -> b) -> [a] -> [(b, [a])]
prepareSubChains getlev =
    drop 1 . reverse . uncurry zip
  . (foldr (\a cs -> min (getlev a) (head cs) : cs) [maxBound] . fmap head . drop 1 &&& id)
  . init . tails . reverse

failInCaseOf :: a -> Bool -> Either a ()
failInCaseOf a = bool (Left a) (Right ())
