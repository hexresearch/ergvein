module Data.Encoding.GolombRice.Strict.Internal where

import           Data.Bits
import           Data.Encoding.GolombRice.Item
import           Data.Foldable
import           Data.Word
import qualified Data.Bitstream                as BS
import           Prelude                 hiding ( null )

-- | Big endian stream of bits
type GolombStream = BS.Bitstream BS.Right

-- | Stream of Golomb-Rice encoded bits. 
data GolombRice a = GolombRice {
  golombRiceP      :: !Int -- ^ Number of bits P in remainder part of encoding
, golombRiceStream :: !GolombStream -- ^ Big endian stream of bits
}

-- | Create empty gololmb rice stream
empty
  :: Int -- ^ Number of bits P in reminder of each element
  -> GolombRice a
empty p = GolombRice p BS.empty
{-# INLINE empty #-}

-- | Create golomb rice stream from single element
singleton
  :: GolombItem a
  => Int  -- ^ Number of bits P in reminder of each element
  -> a -- ^ First element
  -> GolombRice a
singleton p a = GolombRice p bs where bs = foldMap (encodeWord p) $ toWords a

-- | Query if stream contains any data
null :: GolombRice a -> Bool
null = BS.null . golombRiceStream

-- | Encode single word in
encodeWord
  :: Int -- ^ Number of bits P in reminder of each element
  -> Word64 -- ^ Element to write down
  -> GolombStream -- ^ Encoded bits
encodeWord p x = prefix <> reminder
 where
  q      = x `shiftR` p
  prefix = BS.pack $ count q
  count i | i <= 0    = [False]
          | otherwise = True : count (i - 1)
  reminder = BS.pack $ bebits p
  bebits i | i < 0     = []
           | otherwise = x `testBit` i : bebits (i - 1)
