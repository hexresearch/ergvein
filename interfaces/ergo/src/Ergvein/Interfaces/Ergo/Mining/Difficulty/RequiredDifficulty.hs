module Ergvein.Interfaces.Ergo.Mining.Difficulty.RequiredDifficulty where

import Data.Aeson
import Data.Bits
import Data.Bool
import Data.Bytes.Put
import Data.Word
import Safe.Partial

import qualified Data.ByteArray as BA

import Ergvein.Interfaces.Ergo.Common.BigNat
import Ergvein.Interfaces.Ergo.NodeView.History.ErgoHistory

newtype Difficulty = Difficulty { unDifficulty :: BigNat }
  deriving (Eq, Show, ToJSON, FromJSON)

-- | <p>The "compact" format is a representation of a whole number N using an unsigned 32 bit number similar to a
-- floating point format. The most significant 8 bits are the unsigned exponent of base 256. This exponent can
-- be thought of as "number of bytes of N". The lower 23 bits are the mantissa. Bit number 24 (0x800000) represents
-- the sign of N. Therefore, N = (-1^sign) * mantissa * 256^(exponent-3).</p>
--
-- <p>Satoshi's original implementation used BN_bn2mpi() and BN_mpi2bn(). MPI uses the most significant bit of the
-- first byte as sign. Thus 0x1234560000 is compact 0x05123456 and 0xc0de000000 is compact 0x0600c0de. Compact
-- 0x05c0de00 would be -0x40de000000.</p>
--
-- <p>Bitcoin only uses this "compact" format for encoding difficulty targets, which are unsigned 256bit quantities.
-- Thus, all the complexities of the sign bit and using base 256 are probably an implementation accident.</p>
--
decodeCompactBits :: Partial => NBits -> Difficulty
decodeCompactBits = Difficulty . BigNat . fromIntegral . decodeCompactBits'

decodeCompactBits' :: NBits -> Integer
decodeCompactBits' (unNBits -> compact) =
-- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/scala/org/ergoplatform/mining/difficulty/RequiredDifficulty.scala#L32
  if size == 0 then 0 else bigint
  where
    size = fromIntegral $ (compact :: Word32) `shiftR` 24
    msb = (compact :: Word32) `shiftL` 8
    bs = BA.take size . mconcat $ [
           runPutS . putWord32be $ clearBit msb 31
         , BA.replicate (max 0 $ size-1) zeroBits
         ]
    isNegative = testBit msb 31
    bigint = bool id negate isNegative $ rollIntegral . reverse . BA.unpack $ bs
