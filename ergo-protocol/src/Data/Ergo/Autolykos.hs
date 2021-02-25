module Data.Ergo.Autolykos(
    AutolykosSolution(..)
  , AutolykosV2(..)
  , dForV2
  , wForV2
  ) where

import Data.ByteString (ByteString)
import Data.Ergo.BigInt
import Data.Ergo.Crypto
import Data.Persist
import Data.Text (Text, unpack)
import GHC.Generics

data AutolykosSolution = AutolykosSolution
  { minerPubKey   :: !EcPointType
  , oneTimePubKey :: !EcPointType
  , nonce         :: !ByteString -- 8 bytes nonce
  , distance      :: !Integer
  } deriving (Generic, Show, Read, Eq)

instance Persist AutolykosSolution where
  put AutolykosSolution{..} = do
    put minerPubKey
    put oneTimePubKey
    putByteString nonce
    putBigNat distance
  {-# INLINE put #-}
  get = AutolykosSolution
    <$> get
    <*> get
    <*> getBytes 8
    <*> getBigNat
  {-# INLINE get #-}

newtype AutolykosV2 = AutolykosV2 { unAutolykosV2 :: AutolykosSolution }
  deriving (Generic, Show, Read, Eq)

-- dlogGroup :: object SecP256K1 extends BcDlogGroup[SecP256K1Point](CustomNamedCurves.getByName("secp256k1"))
-- wForV2 = CryptoConstants.dlogGroup.generator
wForV2 :: EcPointType
wForV2 = ecPoint "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
  where
    ecPoint s = either (error $ "Failed to parse public key " ++ unpack s) id . decodeEcPointType $ s

dForV2 :: Integer
dForV2 = 0

instance Persist AutolykosV2 where
  put (AutolykosV2 AutolykosSolution{..}) = do
    put minerPubKey
    putByteString nonce
  {-# INLINE put #-}
  get = do
    minerPubKey   <- get
    oneTimePubKey <- pure wForV2
    nonce         <- getBytes 8
    distance      <- pure dForV2
    pure $ AutolykosV2 $ AutolykosSolution{..}
  {-# INLINE get #-}

