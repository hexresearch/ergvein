module Data.Ergo.BigInt(
    BigNat(..)
  , putBigNat
  , getBigNat
  ) where

import Data.Bits
import Data.Persist
import Data.Word
import GHC.Generics

import qualified Data.ByteString as BS

-- | Wrapper around arbitrary precision unsigned integer for serialization
newtype BigNat = BigNat { unBigNat :: Integer }
  deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show, Generic)

-- | Put unsigned integer as (byte length + BE bytes) without leading zeros.
putBigNat :: Integer -> Put ()
putBigNat a = do
  put n
  putByteString bs
  where
    bs = BS.reverse $ BS.unfoldr step a
    n = fromIntegral (BS.length bs) :: Word8
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

-- | Get unsigned integer as (byte length + BE bytes) without leading zeros.
getBigNat :: Get Integer
getBigNat = do
  n :: Word8 <- get
  bs <- getBytes (fromIntegral n)
  pure $ BS.foldl unstep 0 bs
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

instance Persist BigNat where
  put = putBigNat . unBigNat
  {-# INLINE put #-}
  get = fmap BigNat getBigNat
  {-# INLINE get #-}
