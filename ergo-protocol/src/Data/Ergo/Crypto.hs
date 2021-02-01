module Data.Ergo.Crypto(
    EcPointType(..)
  , decodeEcPointType
  , encodeEcPointType
  ) where

import Control.DeepSeq
import Crypto.Secp256k1
import Data.Hashable (Hashable (..))
import Data.Persist
import Data.Text (Text)
import Data.Text.Encoding
import GHC.Generics

import qualified Data.ByteString.Base16 as B16

-- | Public key for secp256k1 curve.
-- Every point is encoded in compressed form (so only X coordinate and sign of Y are stored).
-- Thus for secp256k1 point, 33 bytes are needed. The first bytes is whether equals 2 or 3 depending on the sign of
-- Y coordinate(==2 is Y is positive, ==3, if Y is negative). Other 32 bytes are containing the X coordinate.
-- Special case is infinity point, which is encoded by 33 zeroes.
-- Thus elliptic curve point is always encoded with 33 bytes.
newtype EcPointType = EcPointType { unEcPointType :: PubKey }
  deriving (Eq, Generic, NFData, Read, Show, Hashable)

instance Persist EcPointType where
  put = putByteString . exportPubKey True . unEcPointType
  {-# INLINE put #-}
  get = do
    bs <- getBytes 33
    case importPubKey bs of
      Nothing -> fail $ "Failed to parse SecP256K1 key as EcPointType: " ++ show bs
      Just pk -> pure $ EcPointType pk
  {-# INLINE get #-}

-- | Decode base16 encoded SecP256K1 pub key
decodeEcPointType :: Text -> Either String EcPointType
decodeEcPointType = decode . fst . B16.decode . encodeUtf8

-- | Encode in base16 SecP256K1 pub key
encodeEcPointType :: EcPointType -> Text
encodeEcPointType = decodeUtf8 . B16.encode . encode
