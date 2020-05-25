{-# LANGUAGE BangPatterns #-}
module Data.Encoding.GolombRice.Strict.Mutable.Internal where

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.ByteString                (ByteString)
import           Data.Encoding.GolombRice.Item
import           Data.Word
import           GHC.Generics
import           Prelude                 hiding ( null, read )
import           Safe.Partial

import qualified Data.Foldable                 as F
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Unboxed           as VU
import qualified Prelude                       as P

import qualified Data.Bitstream.C              as BS

-- | Big endian stream of bits
type GolombStream = BS.Bitstream

-- | Stream of Golomb-Rice encoded bits.
data GolombRice a = GolombRice {
  golombRiceP      :: !Int -- ^ Number of bits P in remainder part of encoding
, golombRiceStream :: !GolombStream -- ^ Big endian stream of bits
} deriving (Generic)

instance NFData (GolombRice a)

-- | Create empty gololmb rice stream
empty
  :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> Int -- ^ Preallocated amount of bytes
  -> m (GolombRice a)
empty p n = GolombRice p <$> BS.empty n
{-# INLINE empty #-}

-- | Create golomb rice stream from single element
singleton
  :: (GolombItem a, MonadIO m)
  => Int  -- ^ Number of bits P in reminder of each element
  -> a -- ^ First element
  -> m (GolombRice a)
singleton p a = do
  bs <- encodeWord p (toWord a) =<< BS.empty 8
  pure $ GolombRice p bs
{-# INLINE singleton #-}

-- | Query if stream contains any data
null :: MonadIO m => GolombRice a -> m Bool
null = BS.null . golombRiceStream
{-# INLINE null #-}

-- | Read first encoded element
readMay :: (MonadIO m, GolombItem a) => GolombRice a -> m (Maybe a)
readMay gr = do
  isnull <- null gr
  if isnull then pure Nothing else fmap Just $ read gr
{-# INLINE readMay #-}

-- | Return first encoded element.
read :: (Partial, GolombItem a, MonadIO m) => GolombRice a -> m a
read gr = fmap fromWord $ decodeWord (golombRiceP gr) (golombRiceStream gr)
{-# INLINABLE read #-}

-- | Convert list of items to golomb rice encoding.
fromList :: (GolombItem a, MonadIO m)
  => Int -- ^ Number of bits P in reminder of each element
  -> [a]
  -> m (GolombRice a)
fromList p as = do
  bs <- BS.empty $ 8 * P.length as
  fmap (GolombRice p) $ foldM (flip $ encodeWord p) bs $ fmap toWord as
{-# INLINEABLE fromList #-}

-- | Decode all items from golomb rice encoding.
toList :: (GolombItem a, MonadIO m)
  => GolombRice a
  -> m [a]
toList (GolombRice p s) = fmap fromWord <$> decodeWords p s
{-# INLINEABLE toList #-}

-- | Convert array of items to golomb rice encoding.
fromVector :: (GolombItem a, V.Vector v a, Foldable v, V.Vector v Word64, MonadIO m)
  => Int -- ^ Number of bits P in reminder of each element
  -> v a
  -> m (GolombRice a)
fromVector p as = do
  bs <- BS.empty $ 8 * V.length as
  fmap (GolombRice p) $ foldM (flip $ encodeWord p) bs $ V.map toWord $ as
{-# INLINABLE fromVector #-}

-- | Decode all items from golomb rice encoding.
toVector :: (GolombItem a, V.Vector v a, Foldable v, MonadIO m)
  => GolombRice a
  -> m (v a)
toVector (GolombRice p s) = V.fromList . fmap fromWord <$> decodeWords p s
{-# INLINABLE toVector #-}

-- | Convert array of items to golomb rice encoding.
fromVectorUnboxed :: (GolombItem a, VU.Unbox a, MonadIO m)
  => Int -- ^ Number of bits P in reminder of each element
  -> VU.Vector a
  -> m (GolombRice a)
fromVectorUnboxed p as = do
  bs <- BS.empty $ 8 * VU.length as
  fmap (GolombRice p) . foldM (flip $ encodeWord p) bs . fmap toWord . VU.toList $ as
{-# INLINABLE fromVectorUnboxed #-}

-- | Decode all items from golomb rice encoding.
toVectorUnboxed :: (GolombItem a, VU.Unbox a, MonadIO m)
  => GolombRice a
  -> m (VU.Vector a)
toVectorUnboxed (GolombRice p s) = V.fromList . fmap fromWord <$> decodeWords p s
{-# INLINABLE toVectorUnboxed #-}

-- | Deserialise golomb encoding from bytestring.
fromByteString :: (GolombItem a, MonadIO m)
  => Int -- ^ Number of bits P in reminder of each element
  -> ByteString
  -> m (GolombRice a)
fromByteString p = fmap (GolombRice p) . BS.fromByteString
{-# INLINABLE fromByteString #-}

-- | Serialise golomb encoding to bytestring. The resulting octets will be padded with zeroes.
toByteString :: (GolombItem a, MonadIO m)
  => GolombRice a
  -> m ByteString
toByteString (GolombRice _ s) = BS.toByteString s
{-# INLINABLE toByteString #-}

-- | Foldl over elements with shortcutting that allows to skip rest of set.
foldl :: (GolombItem a, MonadIO m)
  => (b -> a -> m (Shortcut b)) -- ^ Folding function with shorcutting
  -> b -- ^ Starting accumulator
  -> GolombRice a
  -> m b
foldl f b0 (GolombRice p gs) = foldWords p f' b0 gs
  where
    f' !b = f b . fromWord

-- | Encode single word in stream
encodeWord
  :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> Word64 -- ^ Element to write down
  -> GolombStream
  -> m GolombStream
encodeWord p x bs = do
  let q = fromIntegral $ x `shiftR` p
      n = ceiling $ (fromIntegral (q+1+p) :: Double) / 8.0
  bs' <- BS.realloc n bs
  BS.unsafeReplicateBits q True bs'
  BS.unsafeWriteBits False bs'
  BS.unsafeWriteNBits p x bs'
  pure bs'

-- | Decode single word from stream
decodeWord
  :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> GolombStream -- ^ Stream of bits
  -> m Word64
decodeWord p s = do
  q <- fromIntegral <$> BS.countWhile id s
  r <- BS.readNBits p s
  pure $ (q `shiftL` p) + r
{-# INLINABLE decodeWord #-}

-- | Decode to list of words from stream.
decodeWords
  :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> GolombStream -- ^ Stream of bits
  -> m [Word64]
decodeWords p gs = go
  where
    go = do
      e <- BS.null gs
      if e then pure [] else do
        w <- decodeWord p gs
        ws <- w `seq` go
        pure $ w : ws

-- | Helper that tells fold either to continue or stop scanning
data Shortcut a = Next !a | Stop !a
  deriving (Show, Eq, Generic)

-- | Fold over stream of words
foldWords
  :: MonadIO m
  => Int -- ^ Number of bits P in reminder of each element
  -> (a -> Word64 -> m (Shortcut a)) -- ^ Folding function with stop condition
  -> a -- ^ Start accumulator
  -> GolombStream
  -> m a -- ^ End accumulator
foldWords p f a0 s = go a0
  where
    go !a = do
      empty <- BS.null s
      if empty then pure a else do
        w <- decodeWord p s
        sh <- f a w
        case sh of
          Next a' -> go a'
          Stop a' -> pure a'
