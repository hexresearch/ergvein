module Ergvein.Index.Server.Utils where

import Data.Hashable
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as Set

groupMapBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupMapBy keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

mapBy :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapBy keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

uniqueElements :: (Eq a, Hashable a) => [a] -> [a]
uniqueElements = Set.toList . Set.fromList
