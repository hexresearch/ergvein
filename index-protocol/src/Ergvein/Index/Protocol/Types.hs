module Ergvein.Index.Protocol.Types where

import Data.Word
import Data.ByteString
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.ByteString.Builder
import Foreign.Storable

import qualified Data.ByteString.Lazy               as L

data MessageType = Version
                 | VersionACK
                 | FiltersRequest
                 | FiltersResponse
                 | FilterEvent
                 | PeerRequest
                 | PeerResponse
                 | FeeRequest
                 | FeeResponse
                 | IntroducePeer
                 | Reject
                 | Ping
                 | Pong
                  deriving Enum

messageTypeTag :: MessageType -> Word32
messageTypeTag = \case
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

data CurrencyCode = BTC
                  | TBTC
                  | ERGO
                  | TERGO
                  | USDTO
                  | TUSDTO
                  | LTC
                  | TLTC
                  | ZEC
                  | TZEC
                  | CPR
                  | TCPR
                  | DASH
                  | TDASH
                  deriving Enum

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf

scanBlock :: CurrencyCode -> Word32 -> Word64 -> Word64 -> Builder
scanBlock currency version scanHeight height = word32BE (fromIntegral $ fromEnum currency)
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
    msgType = messageTypeTag Version
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
    msgSize = genericSizeOf nonce

msg :: MessageType -> Word32 -> Builder -> Builder
msg msgType msgLength payload = word32BE (messageTypeTag msgType) <> word32BE msgLength <> payload 