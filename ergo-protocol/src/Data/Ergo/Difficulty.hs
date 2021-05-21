module Data.Ergo.Difficulty(
    Difficulty(..)
  , encodeDifficulty
  , decodeDifficulty
  ) where

import Data.Bits
import Data.Persist
import Data.Word
import GHC.Generics

import qualified Data.ByteString as BS

-- | Difficulty is special encoded signed value that is used for target difficulty
--
-- See: https://bitco.in/en/developer-reference#target-nbits
--
-- The compact format is a representation of a whole number \(N\) using an
-- unsigned 32-bit number similar to a floating point format. The most
-- significant 8 bits are the unsigned exponent of base 256. This exponent can
-- be thought of as the number of bytes of \(N\). The lower 23 bits are the
-- mantissa. Bit number 24 represents the sign of \(N\).
--
-- \[
-- N = -1^{sign} \times mantissa \times 256^{exponent-3}
-- \]
--
-- Implementation is taken from https://hackage.haskell.org/package/haskoin-core-0.19.0/docs/src/Haskoin.Block.Common.html#decodeCompact
newtype Difficulty = Difficulty { unDifficulty :: Integer }
  deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show, Generic)

-- | Encode difficulty to compact form. See 'Difficulty' documentation.
encodeDifficulty :: Difficulty -> Word32
encodeDifficulty (Difficulty i) = nCompact
  where
    i' = abs i
    neg = i < 0
    nSize' :: Int
    nSize' = let f 0 = 0
                 f n = 1 + f (n `shiftR` 8)
             in f i'
    nCompact''' :: Word32
    nCompact'''
        | nSize' <= 3 = fromIntegral $ (low64 .&. i') `shiftL` (8 * (3 - nSize'))
        | otherwise = fromIntegral $ low64 .&. (i' `shiftR` (8 * (nSize' - 3)))
    nCompact'' :: Word32
    nSize :: Int
    (nCompact'', nSize)
        | nCompact''' .&. 0x00800000 /= 0 = (nCompact''' `shiftR` 8, nSize' + 1)
        | otherwise = (nCompact''', nSize')
    nCompact' :: Word32
    nCompact' = nCompact'' .|. (fromIntegral nSize `shiftL` 24)
    nCompact :: Word32
    nCompact | neg && (nCompact' .&. 0x007fffff /= 0) = nCompact' .|. 0x00800000
             | otherwise = nCompact'
    low64 :: Integer
    low64 = 0xffffffffffffffff

-- | Decode difficulty from compact form. See 'Difficulty' documentation.
decodeDifficulty :: Word32 -> (Difficulty, Bool) -- ^ 'True' means overflow
decodeDifficulty nCompact = (Difficulty $ if neg then res * (-1) else res, over)
  where
  nSize :: Int
  nSize = fromIntegral nCompact `shiftR` 24
  nWord' :: Word32
  nWord' = nCompact .&. 0x007fffff
  nWord :: Word32
  nWord | nSize <= 3 = nWord' `shiftR` (8 * (3 - nSize))
        | otherwise = nWord'
  res :: Integer
  res | nSize <= 3 = fromIntegral nWord
      | otherwise = fromIntegral nWord `shiftL` (8 * (nSize - 3))
  neg = nWord /= 0 && (nCompact .&. 0x00800000) /= 0
  over = nWord /= 0 && (nSize > 34 ||
                        nWord > 0xff && nSize > 33 ||
                        nWord > 0xffff && nSize > 32)

instance Persist Difficulty where
  put = put . BigEndian . encodeDifficulty
  {-# INLINE put #-}
  get = fmap (fst . decodeDifficulty . unBE) get
  {-# INLINE get #-}
