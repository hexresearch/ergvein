module Ergvein.Text(
    showt
  , text2bs
  , text2json
  , json2text
  , byteStringToBase64Text
  , base64TextToByteString
  ) where

import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Lazy     as BS
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TEE

-- | Helper to transform any showable value to text
showt :: Show a => a -> Text
showt = pack . show

-- | Convert text to lazy bytestring
text2bs :: Text -> BS.ByteString
text2bs = BS.fromStrict . encodeUtf8
{-# INLINABLE text2bs #-}

-- | Parse text as JSON value to haskell type
text2json :: FromJSON a => Text -> Either Text a
text2json = first pack . eitherDecode' . text2bs
{-# INLINABLE text2json #-}

-- | Encode haskell value into JSON and return text of the json
json2text :: ToJSON a => a -> Text
json2text = decodeUtf8 . BS.toStrict . encode
{-# INLINABLE json2text #-}

-- | Convert bytestring to Base64 encoded text
byteStringToBase64Text :: ByteString -> Text
byteStringToBase64Text bs = TE.decodeUtf8With TEE.lenientDecode $ B64.encode bs

-- | Convert Base64 encoded text to bytestring
base64TextToByteString :: Text -> ByteString
base64TextToByteString = B64.decodeLenient . TE.encodeUtf8
