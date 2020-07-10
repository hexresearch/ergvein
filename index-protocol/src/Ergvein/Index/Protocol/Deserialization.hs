module Ergvein.Index.Protocol.Deserialization where

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.ByteString
import Data.Word
import Ergvein.Index.Protocol.Types
import qualified Data.ByteString as BS

word32toMessageType :: Word32 -> Maybe MessageType 
word32toMessageType = \case
  0  -> Just Version
  1  -> Just VersionACK
  2  -> Just FiltersRequest
  3  -> Just FiltersResponse
  4  -> Just FilterEvent
  5  -> Just PeerRequest
  6  -> Just PeerResponse
  7  -> Just FeeRequest
  8  -> Just FeeResponse
  9  -> Just IntroducePeer
  10 -> Just Reject 
  11 -> Just Ping
  12 -> Just Pong
  _  -> Nothing

messageHeaderParser ::  Parser MessageHeader
messageHeaderParser = do
    messageType <- messageTypeParser
    messageSize <- anyWord32be
    pure $ MessageHeader messageType messageSize

messageTypeParser :: Parser MessageType
messageTypeParser = do
  w32 <- anyWord32be
  case word32toMessageType w32 of
   Just messageType -> pure messageType
   _                -> fail "out of message type bounds"

pingMessageParser :: Parser PingMessage
pingMessageParser = anyWord64be

pongMessageParser :: Parser PongMessage
pongMessageParser = anyWord64be

messageParser :: MessageType -> Parser Message
messageParser t = case t of 
  Ping -> PingMessage <$> pingMessageParser
  Pong -> PongMessage <$> pongMessageParser