module Ergvein.Interfaces.Ergo.Modifiers.History.PoPow where

import Control.Applicative
import Control.Arrow
import Data.Foldable as F
import Data.Functor
import Data.List
import qualified Data.Map.Strict as M

import Ergvein.Interfaces.Ergo.Mining.Difficulty.RequiredDifficulty


class IsChainElem b where
  type BlockHash b
  blockHash :: b -> BlockHash b
  mu :: b -> Int

class ( IsChainElem (Element c)
      , Ord (BlockHash (Element c))
      , Foldable (Container c)
      )
    => IsChain c where
  type Element c
  type Container c :: * -> *
  chainElems :: c -> (Container c) (Element c)
  chainLength :: c -> Int
  chainFromList :: [Element c] -> c
  isValidChainAnchoredTo :: c -> c -> Bool
  findDivergingSubchains :: c -> c -> Maybe (c, c)

findDivergingSubchainsWithList :: (IsChain c, Foldable t0, Foldable t1)
    => t0 (Element c) -> t1 (Element c) -> Maybe (c, c)
findDivergingSubchainsWithList a b = fmap (chainFromList *** chainFromList) $
    findDivergingSubListsOn blockHash (F.toList a) (F.toList b)

class IsChain (Chain p) => Proof p where
  type Chain p
  proofPrefix :: p -> Chain p  -- π
  proofSuffix :: p -> Chain p  -- χ
  mkProof :: Chain p -> Chain p -> p

proofChain :: (Proof p, Semigroup (Chain p)) => p -> Chain p
proofChain p = proofSuffix p <> proofPrefix p  --  Notice swapped order of proofPrefix, proofSuffix cause head of chain is leftmost element of most monoids

-- The Verify algorithm for the NIPoPoW protocol
-- 1: function Verify q m,k (P)
-- 2:     π̃ ← (Gen)                                  -- . Trivial anchored blockchain
-- 3:     for (π, χ) ∈ P do                          -- . Examine each proof (π, χ) in P
-- 4:         if validChain(πχ) ∧ |χ| = k ∧ π ≥ m π̃ then
-- 5:             π̃ ← π
-- 6:             χ̃ ← χ                              -- . Update current best
-- 7:         end if
-- 8:     end for
-- 9: return Q̃( χ̃)
-- 10: end function

-- | Verify Q predicate over suffix of selected best proof
-- g - genesis chain
-- q - Q predicate
-- m - security parameter pertaining to the prefix of the proof
-- k - size of the suffix of the proof
niPoPowVerify :: forall p. (Proof p, Monoid (Chain p), Eq (Chain p))
              => Chain p -> (Chain p -> Bool) -> Int -> Int -> [p] -> Bool
niPoPowVerify g q m k = q' . proofSuffix
    . foldl' (\b a -> if proofPrefix a `proofIsMBetterThan` proofPrefix b then a else b) (mkProof g mempty)
    . filter (\p -> chainLength (proofSuffix p) == k && isValidChain (proofChain p))
  where
    q' a | a == mempty = False
         | otherwise   = q a
    proofIsMBetterThan = proofCompare m

    isValidChain :: Proof p => Chain p -> Bool
    isValidChain = isValidChainAnchoredTo g

-- 1: function best-arg m (π, b)
-- 2:     M ← {μ : |π↑^μ {b :}| ≥ m} ∪ {0}
-- 3:     return max μ∈M {2^μ * |π↑^μ {b :}|}
-- 4: end function
-- 5: operator π A ≥m π B
-- 6:     b ← (π A ∩ π B )[−1]
-- 7:     return best-arg m (π A , b) ≥ best-arg m (π B , b)
-- 8: end operator

proofCompare :: forall c. IsChain c => Int -> c -> c -> Bool
proofCompare m a b = maybe False id $ do
    (a', b') <- findDivergingSubchains a b
    pure $ bestArg a' >= bestArg b'
  where
    bestArg :: c -> Int
    bestArg = foldl' max 0
        . fmap (\(k, v) -> 2^k * v)
        . filter (\(k, v) -> v>=m || k==0) . M.toList
        . foldl' (\a b -> M.insertWith (+) (mu b) 1 a) mempty
        . chainElems

findDivergingSubListsOn :: forall a b. (Ord b)
    => (a -> b) -> [a] -> [a] -> Maybe ([a], [a])
findDivergingSubListsOn f a b = do
    let b' = M.fromList $ prepareInits b
    foldr (<|>) Nothing $ reverse (prepareInits a)
                      <&> \(k, as) -> M.lookup k b' <&> (const as &&& id)
  where
    prepareInits :: [a] -> [(b, [a])]
    prepareInits = fmap ((f . head) &&& reverse) . init'
        . foldl' (\cs@(c:_) e -> (e:c):cs) (pure mempty)

    init' [] = []
    init' a = init a
