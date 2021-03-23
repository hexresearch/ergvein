module Ergvein.Either (
    eitherToMaybe
  , eitherToMaybe'
  , allRight
  ) where

import Data.Either

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

eitherToMaybe' :: Either a b -> Maybe a
eitherToMaybe' = either Just (const Nothing)

-- | If the list contains only Right elements returns the entire list
-- Otherwise returns Nothing
allRight :: [Either a b] -> Maybe [b]
allRight xs = if all isRight xs then Just $ rights xs else Nothing
