{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ergvein.Crypto.Hash (
    Hash160(getHash160)
  , Hash192(getHash192)
  , Hash256(getHash256)
  , sha256
  , doubleSHA256
) where

import Data.ByteString.Short   (ShortByteString)
import Data.Hashable           (Hashable)
import Data.Serialize          (Serialize (..))
import Data.String             (IsString, fromString)
import Data.String.Conversions (cs)
import Network.Haskoin.Crypto
import Network.Haskoin.Util    (encodeHex, decodeHex)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize.Get    as Get
import qualified Data.Serialize.Put    as Put
import qualified Text.Read             as R

-- | Type for 192-bit hashes.
newtype Hash192 = Hash192 { getHash192 :: ShortByteString }
  deriving (Eq, Ord, Hashable)

instance Show Hash192 where
  showsPrec _ = shows . encodeHex . BSS.fromShort . getHash192

instance Read Hash192 where
  readPrec = do
    R.String str <- R.lexP
    maybe R.pfail return $ Hash192 . BSS.toShort <$> decodeHex (cs str)

instance IsString Hash192 where
  fromString str =
    case decodeHex $ cs str of
      Nothing -> e
      Just bs ->
        case BS.length bs of
          24 -> Hash192 (BSS.toShort bs)
          _  -> e
    where
      e = error "Could not decode hash from hex string"

instance Serialize Hash192 where
  get = Hash192 <$> Get.getShortByteString 24
  put = Put.putShortByteString . getHash192
