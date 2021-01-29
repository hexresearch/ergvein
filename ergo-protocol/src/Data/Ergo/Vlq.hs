module Data.Ergo.Vlq(
    encodeVlq
  , decodeVlq
  , VarInt(..)
  ) where

import Data.Bits
import Data.Ergo.Shift
import Data.Ergo.ZigZag
import Data.Int
import Data.Persist
import Data.Word

word8 :: Word8 -> Put ()
word8 = put

anyWord8 :: Get Word8
anyWord8 = get

-- | Encode variable length quantity. Last bit of word is tagged with 1 if we
-- have following big endian bytes in stream left.
--
-- https://ergoplatform.org/docs/ErgoTree.pdf
encodeVlq :: (Integral a, ShiftRS a, Bits a) => a -> Put ()
encodeVlq w | w .&. complement 0x7F == 0 = word8 $ fromIntegral w
            | otherwise = do
              word8 $ fromIntegral ((fromIntegral (w .&. 0x7F) :: Word32) .|. 0x80)
              encodeVlq $ shiftRS w 7

-- | Decode variable length quantity. Last bit of word is tagged with 1 if we
-- have following big endian bytes in stream left.
--
-- https://ergoplatform.org/docs/ErgoTree.pdf
decodeVlq :: (Integral a, Bits a) => Get a
decodeVlq = go 0 0
  where
    go !i !acc = do
      w <- fromIntegral <$> anyWord8
      if w .&. complement 0x7F == 0
        then pure $ acc + unsafeShiftL w i
        else go (i+7) (acc + unsafeShiftL (w .&. 0x7F) i)

-- | Encode signed integer with zigzag and then with vlq encoding. Unsigned
-- types are decoded via vlq only.
class VarInt a where
  encodeVarInt :: a -> Put ()
  decodeVarInt :: Get a

instance VarInt Int16 where
  encodeVarInt = encodeVlq . encodeZigZag
  decodeVarInt = fmap decodeZigZag decodeVlq
  {-# INLINE encodeVarInt #-}
  {-# INLINE decodeVarInt #-}

instance VarInt Int32 where
  encodeVarInt = encodeVlq . encodeZigZag
  decodeVarInt = fmap decodeZigZag decodeVlq
  {-# INLINE encodeVarInt #-}
  {-# INLINE decodeVarInt #-}

instance VarInt Int64 where
  encodeVarInt = encodeVlq . encodeZigZag
  decodeVarInt = fmap decodeZigZag decodeVlq
  {-# INLINE encodeVarInt #-}
  {-# INLINE decodeVarInt #-}

instance VarInt Word16 where
  encodeVarInt = encodeVlq
  decodeVarInt = decodeVlq
  {-# INLINE encodeVarInt #-}
  {-# INLINE decodeVarInt #-}

instance VarInt Word32 where
  encodeVarInt = encodeVlq
  decodeVarInt = decodeVlq
  {-# INLINE encodeVarInt #-}
  {-# INLINE decodeVarInt #-}

instance VarInt Word64 where
  encodeVarInt = encodeVlq
  decodeVarInt = decodeVlq
  {-# INLINE encodeVarInt #-}
  {-# INLINE decodeVarInt #-}
