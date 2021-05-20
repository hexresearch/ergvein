module Ergvein.Maybe (
    allJust
  ) where

import Data.Maybe

-- | If the list contains only Just elements returns the entire list
-- Otherwise returns Nothing
allJust :: [Maybe a] -> Maybe [a]
allJust xs = if all isJust xs then Just $ catMaybes xs else Nothing
