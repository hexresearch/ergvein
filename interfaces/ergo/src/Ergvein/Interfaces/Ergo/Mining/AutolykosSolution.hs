module Ergvein.Interfaces.Ergo.Mining.AutolykosSolution where

import Data.Aeson
import Data.Bits
import Data.ByteString (ByteString)
import Data.List    (unfoldr)
import Data.Scientific
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String
import Data.Word
import Numeric.Natural

import qualified Data.ByteString as BS

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Scorex.Util.Package
import Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed

-- | Solution of Autolykos PoW puzzle
--
data AutolykosSolution = AutolykosSolution {
  minerPK   :: EcPointType -- ^ @param pk - miner public key. Should be used to collect block rewards
, onetimePK :: EcPointType -- ^ @param w  - one-time public key. Prevents revealing of miners secret
, nonce     :: Nonce       -- ^ @param n  - nonce
, distance  :: BigNat      -- ^ @param d : BigInt  - distance between
                           -- pseudo-random number, corresponding to nonce `n` and a secret, corresponding
                           -- to `pk`. The lower `d` is, the harder it was to find this solution.
}
  deriving (Eq, Show)

newtype Nonce = Nonce { unNonce :: ByteString }
  deriving (Eq)

instance Show Nonce where
    show = show . toHex . unNonce

instance IsString Nonce where
    fromString = Nonce . fromHex . fromString

instance Serialize Nonce where
    put = S.putByteString . unNonce
    -- val nonce = r.getBytes(8)
    get = Nonce <$> S.getBytes 8

instance ToJSON Nonce where
  toJSON = String . toHex . unNonce
  {-# INLINE toJSON #-}

instance FromJSON Nonce where
  parseJSON = withText "Nonce" $
    either fail (pure . Nonce) . fromHexEitherText
  {-# INLINE parseJSON #-}



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
  parseJSON = withScientific "BigNat" $ \n -> do
    let
      exponent = base10Exponent n
      msg = "found a number with exponent " <> show exponent
          <> ", but it must not be greater than 1024"
    if exponent > 1024
      then fail msg
      else (pure . BigNat . floor $ n)
  {-# INLINE parseJSON #-}

instance FromJSON AutolykosSolution where
  parseJSON = withObject "AutolykosSolution" $ \o -> do
    -- pk <- c.downField("pk").as[EcPointType]
    minerPK <- o .: "pk"  --  EcPointType  -- "pk": "033c46c7fd7085638bf4bc902badb4e5a1942d3251d92d0eddd6fbe5d57e915537",
    -- w <- c.downField("w").as[EcPointType]
    onetimePK <- o .: "w"  --  EcPointType -- "w": "03df646d7f6138aede718a2a4f1a76d4125750e8ab496b7a8a25292d07e14cbadb",
    -- n <- c.downField("n").as[Array[Byte]]
    nonce <- o .: "n"  --      Nonce    -- "n": "0000000a03d0d019",
    -- d <- c.downField("d").as[BigInt]
    distance <- o .: "d"  --   BigNat   -- "d": 2.504075119295696e+63
    -- } yield AutolykosSolution(pk, w, n, d)
    pure AutolykosSolution {..}
  {-# INLINE parseJSON #-}

newtype Secp256k1 = Secp256k1 { unSecp256k1 :: ByteString }
  deriving (Eq)

instance Show Secp256k1 where
    show = show . toHex . unSecp256k1

instance IsString Secp256k1 where
    fromString = Secp256k1 . fromHex . fromString

-- val PublicKeyLength: Byte = 33
instance Serialize Secp256k1 where
    put = put . unSecp256k1
    get = Secp256k1 <$> S.getBytes 33

instance ToJSON Secp256k1 where
  toJSON = String . toHex . unSecp256k1
  {-# INLINE toJSON #-}

instance FromJSON Secp256k1 where
  parseJSON = withText "Secp256k1" $
    either fail (pure . Secp256k1) . fromHexEitherText
  {-# INLINE parseJSON #-}

type EcPointType = Secp256k1

-- object AutolykosSolutionSerializer extends ScorexSerializer[AutolykosSolution] {
instance Serialize AutolykosSolution where
    put AutolykosSolution {..} = do
    -- override def serialize(obj: AutolykosSolution, w: Writer): Unit = {
      -- w.putBytes(groupElemToBytes(obj.pk))
      put minerPK
      -- w.putBytes(groupElemToBytes(obj.w))
      put onetimePK
      -- w.putBytes(obj.n)
      put nonce
      -- val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
      -- w.putUByte(dBytes.length)
      -- w.putBytes(dBytes)
      put distance

    get = do
    -- override def parse(r: Reader): AutolykosSolution = {
      -- val pk = groupElemFromBytes(r.getBytes(PublicKeyLength))
      minerPK   <- get
      -- val w = groupElemFromBytes(r.getBytes(PublicKeyLength))
      onetimePK <- get
      -- val nonce = r.getBytes(8)
      nonce <- get
      -- val dBytesLength = r.getUByte()
      -- val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
      -- AutolykosSolution(pk, w, nonce, d)
      distance <- get
      pure AutolykosSolution {..}
