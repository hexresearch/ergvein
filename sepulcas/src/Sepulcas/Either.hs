module Sepulcas.Either (
    justRight
  , justLeft
  , allRight
  ) where

import Data.Either

justRight :: Either a b -> Maybe b
justRight = either (const Nothing) Just

justLeft :: Either a b -> Maybe a
justLeft = either Just (const Nothing)

-- | If the list contains only Right elements returns the entire list
-- Otherwise returns Nothing
allRight :: [Either a b] -> Maybe [b]
allRight xs = if all isRight xs then Just $ rights xs else Nothing
