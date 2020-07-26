module Ergvein.Index.Protocol.Deserialization where

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Word
import Control.Monad
import Ergvein.Index.Protocol.Types
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as V

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

word32toRejectType :: Word32 -> Maybe RejectCode 
word32toRejectType = \case
  0  -> Just MessageHeaderParsing
  1  -> Just MessageParsing
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

rejectCodeParser :: Parser RejectCode
rejectCodeParser = do
  w32 <- anyWord32be
  case word32toRejectType w32 of
   Just messageType -> pure messageType
   _                -> fail "out of message type bounds"

versionBlocksParser ::  Parser ScanBlock
versionBlocksParser = undefined

messageParser :: MessageType -> Parser Message
messageParser Ping = PingMsg <$> anyWord64be

messageParser Pong = PongMsg <$> anyWord64be

messageParser Reject = RejectMsg . RejectMessage <$> rejectCodeParser

messageParser Version = do
  version       <- anyWord32be
  time          <- fromIntegral <$> anyWord64be
  nonce         <- anyWord64be
  currencies    <- anyWord32be
  versionBlocks <- V.fromList <$> replicateM (fromIntegral currencies) versionBlocksParser

  pure $ VersionMsg $ VersionMessage  
    { versionMsgVersion    = version
    , versionMsgTime       = time
    , versionMsgNonce      = nonce
    , versionMsgScanBlocks = versionBlocks
    }

messageParser VersionACK = pure $ VersionACKMsg VersionACKMessage

messageParser FiltersRequest = undefined

messageParser FiltersResponse = undefined