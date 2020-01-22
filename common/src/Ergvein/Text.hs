module Ergvein.Text(
    showt
  , text2bs
  , text2json
  , json2text
  , showf
  , bs2Hex
  , base64Text2bs
  , bs2Base64Text
  ) where

import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Printf
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Base16    as BS16
import qualified Data.ByteString.Base64    as BS64
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text.Encoding        as TE
import qualified Data.Text.Encoding.Error  as TEE


-- | Helper to transform any showable value to text
showt :: Show a => a -> Text
showt = pack . show

-- | Convert text to lazy bytestring
text2bs :: Text -> BSL.ByteString
text2bs = BSL.fromStrict . encodeUtf8
{-# INLINABLE text2bs #-}

-- | Parse text as JSON value to haskell type
text2json :: FromJSON a => Text -> Either Text a
text2json = first pack . eitherDecode' . text2bs
{-# INLINABLE text2json #-}

-- | Encode haskell value into JSON and return text of the json
json2text :: ToJSON a => a -> Text
json2text = decodeUtf8 . BSL.toStrict . encode
{-# INLINABLE json2text #-}

-- | Print floating point number with fixed precision
showf :: (Floating a, PrintfArg a) => Int -> a -> Text
showf n = pack . printf ("%." <> show n <> "f")

bs2Hex :: BS.ByteString -> Text
bs2Hex = TE.decodeUtf8 . BS16.encode

-- | Convert Base64 encoded text to ByteString
base64Text2bs :: Text -> ByteString
base64Text2bs = BS64.decodeLenient . TE.encodeUtf8

-- | Convert ByteString to Base64 encoded text
bs2Base64Text :: ByteString -> Text
bs2Base64Text bs = TE.decodeUtf8With TEE.lenientDecode $ BS64.encode bs
