module Ergvein.Interfaces.Ergo.Mining.AutolykosSolution where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import Data.String

import qualified Data.ByteString as BS

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Common.BigNat
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

instance ToJSON AutolykosSolution where
  toJSON AutolykosSolution {..} = object [
      -- "pk" -> s.pk.asJson,
      "pk" .= toJSON minerPK
      -- "w" -> s.w.asJson,
    , "w" .= toJSON onetimePK
      -- "n" -> Algos.encode(s.n).asJson,
    , "n" .= toJSON nonce
      -- "d" -> s.d.asJson(bigIntEncoder)
    , "d" .= toJSON distance
    ]
  {-# INLINE toJSON #-}

instance ToJSON Nonce where
  toJSON = String . toHex . unNonce
  {-# INLINE toJSON #-}

instance FromJSON Nonce where
  parseJSON = withText "Nonce" $
    either fail (pure . Nonce) . fromHexTextEither
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
    either fail (pure . Secp256k1) . fromHexTextEither
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
