{-# LANGUAGE BangPatterns #-}
module Data.Encoding.GolombRice.Strict.Internal where

import           Data.Bits
import           Data.ByteString                (ByteString)
import           Data.Encoding.GolombRice.Item
import           Data.Word
import           GHC.Generics
import           Prelude                 hiding ( null, head )
import           Safe.Partial
import qualified Data.Bitstream                as BS
import qualified Data.Foldable                 as F
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Unboxed           as VU
import qualified Prelude                       as P

-- | Big endian stream of bits
type GolombStream = BS.Bitstream BS.Right

-- | Stream of Golomb-Rice encoded bits.
data GolombRice a = GolombRice {
  golombRiceP      :: !Int -- ^ Number of bits P in remainder part of encoding
, golombRiceStream :: !GolombStream -- ^ Big endian stream of bits
} deriving (Show, Generic)

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

-- | Convert array of items to golomb rice encoding.
fromVectorUnboxed :: (GolombItem a, VU.Unbox a)
  => Int -- ^ Number of bits P in reminder of each element
  -> VU.Vector a
  -> GolombRice a
fromVectorUnboxed p = GolombRice p . foldMap (encodeWord p) . fmap toWord . VU.toList
{-# INLINABLE fromVectorUnboxed #-}

-- | Decode all items from golomb rice encoding.
toVectorUnboxed :: (GolombItem a, VU.Unbox a)
  => GolombRice a
  -> VU.Vector a
toVectorUnboxed (GolombRice p s) = V.fromList . fmap fromWord $ decodeWords p s
{-# INLINABLE toVectorUnboxed #-}

-- | Deserialise golomb encoding from bytestring.
fromByteString :: GolombItem a
  => Int -- ^ Number of bits P in reminder of each element
  -> ByteString
  -> GolombRice a
fromByteString p = GolombRice p . BS.fromByteString
{-# INLINABLE fromByteString #-}

-- | Serialise golomb encoding to bytestring. The resulting octets will be padded with zeroes.
toByteString :: GolombItem a
  => GolombRice a
  -> ByteString
toByteString (GolombRice _ s) = BS.toByteString s
{-# INLINABLE toByteString #-}

-- | Foldl over elements with shortcutting that allows to skip rest of set.
foldl :: GolombItem a
  => (b -> a -> Shortcut b) -- ^ Folding function with shorcutting
  -> b -- ^ Starting accumulator
  -> GolombRice a
  -> b
foldl f b0 (GolombRice p gs) = foldWords p f' b0 gs
  where
    f' !b = f b . fromWord

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

-- | Helper that tells fold either to continue or stop scanning
data Shortcut a = Next !a | Stop !a
  deriving (Show, Eq, Generic)

-- | Fold over stream of words
foldWords
  :: Int -- ^ Number of bits P in reminder of each element
  -> (a -> Word64 -> Shortcut a) -- ^ Folding function with stop condition
  -> a -- ^ Start accumulator
  -> GolombStream
  -> a -- ^ End accumulator
foldWords p f a0 gs
  | BS.null gs = a0
  | otherwise = go a0 gs
  where
    go !a !s = let
      (w, s') = decodeWord p s
      sh = f a w
      in case sh of
          Next a' -> if BS.null s' then a' else go a' s'
          Stop a' -> a'
