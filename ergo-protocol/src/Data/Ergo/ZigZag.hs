module Data.Ergo.ZigZag(
    ZigZag(..)
  ) where

import Data.Bits
import Data.Int
import Data.Word

-- | ZigZag encodes signed integers
-- into values that can be efficiently encoded with varint.  (Otherwise,
-- negative values must be sign-extended to 64 bits to be varint encoded,
-- thus always taking 10 bytes on the wire.)
--
-- Decoding: http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
-- Encoding: https://github.com/protocolbuffers/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedOutputStream.java#L949
class ZigZag a b | a -> b, b -> a where
  encodeZigZag :: a -> b
  decodeZigZag :: b -> a

instance ZigZag Int16 Word16 where
  encodeZigZag n = (fromIntegral (abs n) `unsafeShiftL` 1) `xor` (fromIntegral n `unsafeShiftR` 15)
  decodeZigZag n = let
    x = fromIntegral $ n `unsafeShiftR` 1
    s = fromIntegral (n .&. 1)
    in if s == (0 :: Int) then x else negate x

instance ZigZag Int32 Word32 where
  encodeZigZag n = (fromIntegral (abs n) `unsafeShiftL` 1) `xor` (fromIntegral n `unsafeShiftR` 31)
  decodeZigZag n = let
    x = fromIntegral $ n `unsafeShiftR` 1
    s = fromIntegral (n .&. 1)
    in if s == (0 :: Int) then x else negate x

instance ZigZag Int64 Word64 where
  encodeZigZag n = (fromIntegral (abs n) `unsafeShiftL` 1) `xor` (fromIntegral n `unsafeShiftR` 63)
  decodeZigZag n = let
    x = fromIntegral $ n `unsafeShiftR` 1
    s = fromIntegral (n .&. 1)
    in if s == (0 :: Int) then x else negate x
