module Data.Ergo.BigInt(
    BigNat(..)
  , encodeBigNat
  , putBigNat
  , decodeBigNat
  , getBigNat
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Persist
import Data.Word
import GHC.Generics

import qualified Data.ByteString as BS

-- | Wrapper around arbitrary precision unsigned integer for serialization
newtype BigNat = BigNat { unBigNat :: Integer }
  deriving (Enum, Eq, Integral, Num, Ord, Read, Real, Show, Generic)

-- | Encode big unsigned integer as BE bytes
encodeBigNat :: Integer -> ByteString
encodeBigNat = BS.reverse . BS.unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

-- | Put unsigned integer as (byte length + BE bytes) without leading zeros.
putBigNat :: Integer -> Put ()
putBigNat a = do
  put n
  putByteString bs
  where
    bs = encodeBigNat a
    n = fromIntegral (BS.length bs) :: Word8

-- | Decode BE unsigned integer without leading zeros.
decodeBigNat :: ByteString -> Integer
decodeBigNat = BS.foldl unstep 0
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b

-- | Get unsigned integer as (byte length + BE bytes) without leading zeros.
getBigNat :: Get Integer
getBigNat = do
  n :: Word8 <- get
  bs <- getBytes (fromIntegral n)
  pure $ decodeBigNat bs

instance Persist BigNat where
  put = putBigNat . unBigNat
  {-# INLINE put #-}
  get = fmap BigNat getBigNat
  {-# INLINE get #-}
