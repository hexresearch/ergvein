module Ergvein.Interfaces.Ergo.Mining.AutolykosSolution where

import Data.ByteString
import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S
import GHC.Natural

import Ergvein.Interfaces.Ergo.Scorex.Util.Serialization.VLQLengthPrefixed

-- | Solution of Autolykos PoW puzzle
--
data AutolykosSolution = AutolykosSolution {
  minerPK   :: EcPointType -- ^ @param pk - miner public key. Should be used to collect block rewards
, onetimePK :: EcPointType -- ^ @param w  - one-time public key. Prevents revealing of miners secret
, nonce     :: Nonce       -- ^ @param n  - nonce
, distance  :: Integer     -- ^ @param d : BigInt  - distance between
                           -- pseudo-random number, corresponding to nonce `n` and a secret, corresponding
                           -- to `pk`. The lower `d` is, the harder it was to find this solution.
}
  deriving (Eq)

newtype Nonce = Nonce { unNonce :: ByteString }
  deriving (Eq)

instance Serialize Nonce where
    put = S.putByteString . unNonce
    -- val nonce = r.getBytes(8)
    get = Nonce <$> S.getBytes 8


newtype BigInt = BigInt { unBigInt :: Integer }
  deriving (Eq)

-- https://github.com/bigfatbrowncat/avian-pack.android.external.bouncycastle/blob/bd63be61caf85120ee69cda508a35580a230d57c/bcprov/src/main/java/org/bouncycastle/util/BigIntegers.java#L20
instance Serialize BigInt where
    -- val dBytes = BigIntegers.asUnsignedByteArray(obj.d.bigInteger)
    -- w.putUByte(dBytes.length)
    -- w.putBytes(dBytes)
    put = put . OneByteLengthPrefixed . encode @Natural . fromIntegral . unBigInt
    get = do
        -- val dBytesLength = r.getUByte()
        -- val d = BigInt(BigIntegers.fromUnsignedByteArray(r.getBytes(dBytesLength)))
        OneByteLengthPrefixed bs <- get
        BigInt . fromIntegral @Natural <$> (either fail pure $ decode bs)

newtype Secp256k1 = Secp256k1 { unSecp256k1 :: ByteString }
  deriving (Eq)

type EcPointType = Secp256k1

-- val PublicKeyLength: Byte = 33
instance Serialize Secp256k1 where
    put = put . unSecp256k1
    get = Secp256k1 <$> S.getBytes 33

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
      put $ BigInt distance

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
      BigInt distance <- get
      pure AutolykosSolution {..}
