module Data.Encoding.GolombRice.Item
  ( GolombItem(..)
  )
where

import           Data.Bits
import           Data.Int
import           Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

-- | Golomb encoder can store anything that can be converted
-- to 'Word64'.
--
-- The following rule must hold:
--
-- @
--   fromWord . toWord == id
-- @
class GolombItem a where
  -- | Convert element to word
  toWord   :: a -> Word64
  -- | Parse element from first N words and return reminder
  fromWord :: Word64 -> a

instance GolombItem Word64 where
  toWord = id
  {-# INLINE toWord #-}
  fromWord = id
  {-# INLINE fromWord #-}

instance GolombItem Word32 where
  toWord = fromIntegral
  {-# INLINE toWord #-}
  fromWord = fromIntegral
  {-# INLINE fromWord #-}

instance GolombItem Word where
  toWord = fromIntegral
  {-# INLINE toWord #-}
  fromWord = fromIntegral
  {-# INLINE fromWord #-}
