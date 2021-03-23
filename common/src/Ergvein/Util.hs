module Ergvein.Util(
    eitherToMaybe
  , eitherToMaybe'
  , allJust
  , allRight
  ) where

import Data.Either
import Data.Maybe

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

eitherToMaybe' :: Either a b -> Maybe a
eitherToMaybe' = either Just (const Nothing)

-- | If the list contains only Just elements returns the entire list
-- Otherwise returns Nothing
allJust :: [Maybe a] -> Maybe [a]
allJust xs = if all isJust xs then Just $ catMaybes xs else Nothing

-- | If the list contains only Right elements returns the entire list
-- Otherwise returns Nothing
allRight :: [Either a b] -> Maybe [b]
allRight xs = if all isRight xs then Just $ rights xs else Nothing
