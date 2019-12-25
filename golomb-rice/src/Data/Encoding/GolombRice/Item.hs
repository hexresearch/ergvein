module Data.Encoding.GolombRice.Item
  ( GolombItem(..)
  )
where

import           Data.Word

-- | Golomb encoder can store anything that can be converted
-- to 'Word64'.
--
-- The following rule must hold:
--
-- @
--   fromWords (toWords a) == (a, [])
-- @
class GolombItem a where
  -- | Convert element to words
  toWords   :: a -> [Word64]
  -- | Parse element from first N words and return reminder
  fromWords :: [Word64] -> (a, [Word64])

instance GolombItem Word64 where
  toWords = pure
  {-# INLINE toWords #-}
  fromWords []       = (0, [])
  fromWords (a : as) = (a, as)
  {-# INLINE fromWords #-}
