module Ergvein.Crypto.SHA256 where

import Crypto.Hash
import Data.Text

import qualified Data.ByteArray          as BA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base16  as BS16
import qualified Data.ByteString.Short   as BSS
import qualified Data.Serialize          as S
import qualified Data.Text.Encoding      as E
import qualified Data.Serialize.Put      as Put
import qualified Data.Serialize.Get      as Get

newtype Hash256 = Hash256 { getHash256 :: BSS.ShortByteString }
    deriving (Eq, Ord)

instance S.Serialize Hash256 where
    get = Hash256 <$> Get.getShortByteString 32
    put = Put.putShortByteString . getHash256

-- | Compute two rounds of SHA-256.
doubleSHA256 :: BA.ByteArrayAccess b => b -> Hash256
doubleSHA256 = Hash256 . BSS.toShort . BA.convert . hashWith SHA256 . hashWith SHA256

-- | Encode SHA256 as string of human-readable hex characters.
encodeSHA256Hex :: Hash256 -> Text
encodeSHA256Hex = E.decodeUtf8 . BS16.encode . BS.reverse . S.encode
