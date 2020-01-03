{-# LANGUAGE BangPatterns #-}
module Data.Encoding.GolombRice.Strict.Internal where

import           Data.Bits
import           Data.Encoding.GolombRice.Item
import           Data.Word
import           Prelude                 hiding ( null, head )
import           Safe.Partial
import qualified Data.Bitstream                as BS
import qualified Data.Foldable                 as F
import qualified Prelude                       as P

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
singleton p a = GolombRice p bs where bs = encodeWord p $ toWord a
{-# INLINE singleton #-}

-- | Query if stream contains any data
null :: GolombRice a -> Bool
null = BS.null . golombRiceStream
{-# INLINE null #-}

-- | Return first encoded element
headMay :: GolombItem a => GolombRice a -> Maybe a
headMay gr
  | null gr   = Nothing
  | otherwise = Just $ head gr
{-# INLINE headMay #-}

-- | Return first encoded element.
head :: (Partial, GolombItem a) => GolombRice a -> a
head gr = fromWord . P.head $ decodeWords (golombRiceP gr) (golombRiceStream gr)
{-# INLINABLE head #-}

-- | Encode single word in stream
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
  reminder = BS.fromNBits p x
  -- reminder = go (p-1)
  -- go i
  --   | i < 0     = []
  --   | otherwise = testBit x i : go (i-1)

-- | Decode single word from stream
decodeWord
  :: Int -- ^ Number of bits P in reminder of each element
  -> GolombStream -- ^ Stream of bits
  -> (Word64, GolombStream) -- ^ Result and leftover
decodeWord p s = (x, leftover)
 where
  prefix   = BS.takeWhile id s
  postfix  = BS.drop 1 . BS.dropWhile id $ s
  leftover = BS.drop p postfix
  q        = fromIntegral $ BS.length prefix
  r        = BS.toBits . BS.take p $ postfix
  x        = (q `shiftL` p) + r
  -- toBits   = go 0 0
  -- go !i !acc bs
  --   | i >= p = acc
  --   | otherwise = let
  --     a = if bs V.! i then 1 `shiftL` fromIntegral (p-i-1) else 0
  --     acc' = acc + a
  --     in go (i+1) acc' bs

-- | Decode to list of words from stream.
decodeWords
  :: Int -- ^ Number of bits P in reminder of each element
  -> GolombStream -- ^ Stream of bits
  -> [Word64]
decodeWords p = go
  where
    go !s = let
      (w, s') = decodeWord p s
      in if BS.null s' then [w] else w `seq` w : go s'
