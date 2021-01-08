module Ergvein.Interfaces.Ergo.Common.BigNat where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Bits
import Data.List    (unfoldr)
import Data.Scientific
import Data.Serialize                     as S (Serialize (..), get, put)
import Data.String
import Data.Word
import Numeric.Natural

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE

import Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed

newtype BigNat = BigNat { unBigNat :: Natural }
  deriving (Eq)

instance Show BigNat where
    show = show . unBigNat

instance IsString BigNat where
    fromString = BigNat . read

-- https://github.com/bigfatbrowncat/avian-pack.android.external.bouncycastle/blob/bd63be61caf85120ee69cda508a35580a230d57c/bcprov/src/main/java/org/bouncycastle/util/BigIntegers.java#L20
instance Serialize BigNat where
    -- val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    -- w.putUByte(dBytes.length)
    -- w.putBytes(dBytes)
    put = put . OneByteLengthPrefixed . BS.pack . unrollIntegral . unBigNat
    -- val dBytesLength = r.getUByte()
    -- val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
    -- BigInt . fromIntegral @Natural <$> (either fail pure $ decode bs)
    get = BigNat . rollIntegral . BS.unpack . unOneByteLengthPrefixed <$> get

--
-- Fold and unfold an Integral to and from a list of its bytes
--
unrollIntegral :: (Integral a, Bits a) => a -> [Word8]
unrollIntegral = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

rollIntegral :: (Integral a, Bits a) => [Word8] -> a
rollIntegral   = foldr unstep 0
  where
    -- unstep :: (Integral b, Num a, Bits a) => b -> a -> a
    unstep b a = a `shiftL` 8 .|. fromIntegral b

instance ToJSON BigNat where
  toJSON = toJSON . fromIntegral @_ @Scientific . unBigNat
  {-# INLINE toJSON #-}

instance FromJSON BigNat where
  parseJSON j = parseScientific j <|> parseStringScientific j
  {-# INLINE parseJSON #-}

parseStringScientific :: Value -> Parser BigNat
parseStringScientific = withText "BigNat string" $ either fail pure . eitherDecode . BSL.fromStrict . TE.encodeUtf8
{-# INLINE parseStringScientific #-}

parseScientific :: Value -> Parser BigNat
parseScientific =
  withScientific "BigNat" $ \n -> do
    let
      expnt = base10Exponent n
      msg = "found a number with exponent " <> show expnt
          <> ", but it must not be greater than 1024"
    if expnt > 1024
      then fail msg
      else (pure . BigNat . floor $ n)
{-# INLINE parseScientific #-}
