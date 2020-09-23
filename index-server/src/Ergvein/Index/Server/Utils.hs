module Ergvein.Index.Server.Utils where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.Foldable
import Data.Hashable
import Data.Word
import System.Timeout

import qualified Data.HashSet as Set
import qualified Data.List as L
import qualified Data.Map.Strict as Map

groupMapBy :: Ord k => (v -> k) -> [v] -> Map.Map k [v]
groupMapBy keySelector = Map.fromListWith (++) . fmap (\v-> (keySelector v , [v]))

mapBy :: Ord k => (v -> k) -> [v] -> Map.Map k v
mapBy keySelector = Map.fromList . fmap (\v-> (keySelector v , v))

uniqueElements :: (Eq a, Hashable a) => [a] -> [a]
uniqueElements = Set.toList . Set.fromList

uniqueWithCount :: Ord a => [a] -> [(a, Word32)]
uniqueWithCount = Map.toList . L.foldl' (\m a -> Map.insertWith (+) a 1 m) Map.empty

cancelableDelay :: TVar Bool -> Int -> IO ()
cancelableDelay cancellationToken delay = void $ timeout delay $ atomically $ readTVar cancellationToken >>= (`unless` retry)

mkChunks :: Int -> [a] -> [[a]]
mkChunks n vals = mkChunks' [] vals
  where
     mkChunks' acc xs = case xs of
       [] -> acc
       _ -> let (a,b) = splitAt n xs in mkChunks' (acc ++ [a]) b
