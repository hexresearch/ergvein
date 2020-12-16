module Data.Ergo.Protocol.Decoder(
    decodeMessage
  ) where

import Data.ByteString (ByteString)
import Data.Ergo.Protocol.Types

-- | Perform parsing of whole message from bytestring without remainder
decodeMessage :: Network -> ByteString -> Either String Message
decodeMessage = undefined
