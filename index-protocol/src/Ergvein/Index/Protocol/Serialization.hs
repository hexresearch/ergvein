module Ergvein.Index.Protocol.Serialization where

import Data.ByteString.Builder
import Data.Word
import Ergvein.Index.Protocol.Types
import qualified Data.Vector as V

messageTypeToWord32 :: MessageType -> Word32 
messageTypeToWord32 = \case
  Version         -> 0 
  VersionACK      -> 1 
  FiltersRequest  -> 2 
  FiltersResponse -> 3 
  FilterEvent     -> 4 
  PeerRequest     -> 5 
  PeerResponse    -> 6 
  FeeRequest      -> 7 
  FeeResponse     -> 8 
  IntroducePeer   -> 9 
  Reject          -> 10 
  Ping            -> 11
  Pong            -> 12

rejectTypeToWord32 :: RejectCode -> Word32 
rejectTypeToWord32 = \case
  MessageHeaderParsing -> 0
  MessageParsing       -> 1

{-
scanBlock :: CurrencyCode -> Word32 -> Word64 -> Word64 -> Builder
scanBlock currency version scanHeight height = word32BE (undefined currency)
                                            <> word32BE version
                                            <> word64BE scanHeight
                                            <> word64BE height

verMsg :: Word32 ->  Word64 -> Word64 -> V.Vector Builder -> Builder
verMsg version time nonce scanBlocks = 
  msg Version msgSize $ word32BE version
                     <> word64BE time
                     <> word64BE nonce
                     <> word32BE currenciesAmount <> V.foldl (<>) mempty scanBlocks
  where
    scanBlockSize = 24
    currenciesAmount = fromIntegral $ V.length scanBlocks
    msgSize = genericSizeOf time + genericSizeOf nonce + currenciesAmount * scanBlockSize

verACKMsg :: Builder
verACKMsg = msg VersionACK msgSize mempty
  where
    msgSize = 0

pingMsg :: Word64 -> Builder
pingMsg nonce = msg Ping msgSize $ word64BE nonce
  where
    msgSize = genericSizeOf nonce

pongMsg :: Word64 -> Builder
pongMsg nonce = msg Pong msgSize $ word64BE nonce
  where
    msgSize = genericSizeOf nonce-}

messageBase :: MessageType -> Word32 -> Builder -> Builder
messageBase msgType msgLength payload = word32BE (messageTypeToWord32 msgType) <> word32BE msgLength <> payload

messageBuilder :: Message -> Builder

messageBuilder (PingMsg msg) = messageBase Ping msgSize $ word64BE msg
  where
    msgSize = genericSizeOf msg

messageBuilder (PongMsg   msg) = messageBase Pong msgSize $ word64BE msg
  where
    msgSize = genericSizeOf msg

messageBuilder (RejectMsg msg) = messageBase Reject msgSize $ word32BE $ rejectTypeToWord32 $ rejectMsgCode msg
  where
    msgSize = genericSizeOf $ rejectTypeToWord32 $ rejectMsgCode msg