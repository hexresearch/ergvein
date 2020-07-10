module Ergvein.Index.Protocol.Deserialization where

import Ergvein.Index.Protocol.Types
import qualified Data.ByteString as BS
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Word
import Data.ByteString
import qualified Data.Attoparsec.ByteString as ABS

readMessageInfo :: ByteString -> ABS.Result (MessageType, Word32)
readMessageInfo str = (`parse` str) $ do
    messageType <- messageTypeParser
    messageSize <- anyWord32be
    pure (messageType, messageSize)

messageTypeParser :: Parser MessageType
messageTypeParser = do
  w32 <- anyWord32be
  case word32toMessageType w32 of
   Just messageType -> pure messageType
   _                -> fail "out of message type bounds"