module Ergvein.Text(
    showt
  , text2bs
  , text2json
  , json2text
  ) where

import Data.Aeson
import Data.Bifunctor
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Lazy as BS

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
