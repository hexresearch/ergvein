module Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed where

import Control.Monad
import Data.Bits
import Data.Bool
import Data.Int
import Data.Serialize as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get as S
import Data.Word

import qualified Data.ByteString as BS

newtype VLQLengthPrefixed a = VLQLengthPrefixed a
newtype VLQInt32 = VLQInt32 { unVLQInt32 :: Int32 }
newtype VLQWord32 = VLQWord32 { unVLQWord32 :: Word32 }

-- | @note Uses ZigZag encoding. Should be used to decode '''only''' a value that was previously
--       encoded with [[VLQByteBufferWriter.putInt]].
-- @see [[https://en.wikipedia.org/wiki/Variable-length_quantity]]
instance Serialize VLQInt32 where
  -- @inline override def putInt(x: Int): this.type = putULong(encodeZigZagInt(x))
  put = put . VLQWord32 . toZigZag . unVLQInt32

  -- @inline override def getInt(): Int = { decodeZigZagInt(getULong().toInt) }
  get = VLQInt32 . fromZigZag . unVLQWord32 <$> get

--
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Int.html
-- For coercing between any two integer types, use fromIntegral, which is
-- specialized for all the common cases so should be fast enough. Coercing word
-- types (see Data.Word) to and from integer types preserves representation,
-- not sign.
--
-- Note:  the right-shift must be arithmetic
-- source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L934
-- (n << 1) ^ (n >> 31)
--
-- For signed integers (any IntX) `shiftR` does arithmetic shift
-- ```
-- λ showBits = putStrLn . mconcat . fmap show . reverse . fmap (bool 0 1) . toListOf bits
-- λ showBits $ ((0 `setBit` 7) :: Int8) `shiftR` 1
-- 11000000
-- ```
toZigZag :: Int32 -> Word32
toZigZag a = fromIntegral $ ((a :: Int32) `shiftR` 31) `xor` (a `shiftL` 1)

-- source: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
-- (n >>> 1) ^ -(n & 1)
-- For unsigned integers (any WordX) `shiftR` does logical shift
-- ```
-- λ showBits $ ((0 `setBit` 7) :: Word8) `shiftR` 1
-- 01000000
-- ```
fromZigZag :: Word32 -> Int32
fromZigZag a = fromIntegral $ ((a :: Word32) `shiftR` 1) `xor` (onesForOdd a)
  where
    onesForOdd = flip (bool id complement) 0 . odd

instance Serialize VLQWord32 where
  put = undefined
  get = undefined

instance Serialize a => Serialize (VLQLengthPrefixed [a]) where
  put (VLQLengthPrefixed as) = do
      -- w.putInt(obj.interlinks.size)
      put . VLQInt32 . fromIntegral . length $ as
      -- obj.interlinks.foreach(x => w.putBytes(idToBytes(x)))
      mapM_ put as
  get = do
      n <- fromIntegral . unVLQInt32 <$> get
      VLQLengthPrefixed <$> replicateM n get

instance Serialize (VLQLengthPrefixed BS.ByteString) where
  put (VLQLengthPrefixed bs) = do
      -- w.putInt(headerBytes.length)
      put . VLQInt32 . fromIntegral . BS.length $ bs
      -- w.putBytes(headerBytes)
      put bs
  get = do
      n <- fromIntegral . unVLQInt32 <$> get
      VLQLengthPrefixed <$> S.getBytes n
