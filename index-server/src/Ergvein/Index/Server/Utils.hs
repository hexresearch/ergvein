module Ergvein.Index.Server.Utils where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Foldable
import Data.Hashable

import qualified Data.Map.Strict as Map
import qualified Data.HashSet as Set

groupMapBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupMapBy keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

mapBy :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapBy keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

uniqueElements :: (Eq a, Hashable a) => [a] -> [a]
uniqueElements = Set.toList . Set.fromList

cancelableDelay :: TVar Bool -> Int -> IO ()
cancelableDelay cancellationToken delay = do
  delay <- registerDelay delay
  atomically $ asum
    [ readTVar delay >>= check'
    , readTVar cancellationToken >>= check'
    ]
  where
    check' var' = when var' retry