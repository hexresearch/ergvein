{-# LANGUAGE BangPatterns #-}
module Data.Encoding.GolombRice.Strict.Internal where

import           Data.Bits
import           Data.Encoding.GolombRice.Item
import           Data.Word
import           Prelude                 hiding ( null, head )
import           Safe.Partial
import qualified Data.Bitstream                as BS
import qualified Data.Foldable                 as F
import qualified Data.Vector.Generic           as V
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

-- | Convert list of items to golomb rice encoding.
fromList :: GolombItem a
  => Int -- ^ Number of bits P in reminder of each element
  -> [a]
  -> GolombRice a
fromList p = GolombRice p . foldMap (encodeWord p) . fmap toWord
{-# INLINEABLE fromList #-}

-- | Decode all items from golomb rice encoding.
toList :: GolombItem a
  => GolombRice a
  -> [a]
toList (GolombRice p s) = fmap fromWord $ decodeWords p s
{-# INLINEABLE toList #-}

-- | Convert array of items to golomb rice encoding.
fromVector :: (GolombItem a, V.Vector v a, Foldable v, V.Vector v Word64)
  => Int -- ^ Number of bits P in reminder of each element
  -> v a
  -> GolombRice a
fromVector p = GolombRice p . foldMap (encodeWord p) . V.map toWord
{-# INLINABLE fromVector #-}

-- | Decode all items from golomb rice encoding.
toVector :: (GolombItem a, V.Vector v a, Foldable v)
  => GolombRice a
  -> v a
toVector (GolombRice p s) = V.fromList . fmap fromWord $ decodeWords p s
{-# INLINABLE toVector #-}

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

-- | Decode to list of words from stream.
decodeWords
  :: Int -- ^ Number of bits P in reminder of each element
  -> GolombStream -- ^ Stream of bits
  -> [Word64]
decodeWords p gs
  | BS.null gs = []
  | otherwise = go gs
  where
    go !s = let
      (w, s') = decodeWord p s
      in if BS.null s' then [w] else w `seq` w : go s'
