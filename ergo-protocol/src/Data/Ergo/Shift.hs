module Data.Ergo.Shift(
    ShiftRS(..)
  ) where

import Data.Bits
import Data.Int
import Data.Word

-- | Shifting right with stopping propogation of sign bit
class ShiftRS a where
  shiftRS :: a -> Int -> a

instance ShiftRS Word64 where
  shiftRS = unsafeShiftR
  {-# INLINE shiftRS #-}

instance ShiftRS Int64 where
  shiftRS a i
    | a >= 0 = unsafeShiftR a i
    | otherwise = unsafeShiftR a i + (unsafeShiftL 2 (complement i))
  {-# INLINE shiftRS #-}

instance ShiftRS Word32 where
  shiftRS = unsafeShiftR
  {-# INLINE shiftRS #-}

instance ShiftRS Int32 where
  shiftRS a i
    | a >= 0 = unsafeShiftR a i
    | otherwise = unsafeShiftR a i + (unsafeShiftL 2 (complement i))
  {-# INLINE shiftRS #-}

instance ShiftRS Word16 where
  shiftRS = unsafeShiftR
  {-# INLINE shiftRS #-}

instance ShiftRS Int16 where
  shiftRS a i
    | a >= 0 = unsafeShiftR a i
    | otherwise = unsafeShiftR a i + (unsafeShiftL 2 (complement i))
  {-# INLINE shiftRS #-}
