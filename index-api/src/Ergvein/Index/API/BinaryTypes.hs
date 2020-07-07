module Ergvein.Index.API.BinaryTypes where

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

currencyTag :: CurrencyCode -> Word32
currencyTag = \case
  BTC    -> 0
  TBTC   -> 1
  ERGO   -> 2
  TERGO  -> 3
  USDTO  -> 4
  TUSDTO -> 5
  LTC    -> 6
  TLTC   -> 7
  ZEC    -> 8
  TZEC   -> 9
  CPR    -> 10
  TCPR   -> 11
  DASH   -> 12
  TDASH  -> 13

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf

scanBlock :: CurrencyCode -> Word32 -> Word64 -> Word64 -> Builder
scanBlock currency version scanHeight height = word32BE (currencyTag currency)
                                            <> word32BE version
                                            <> word64BE scanHeight
                                            <> word64BE height

verMsg :: Word32 ->  Word64 -> Word64 -> V.Vector Builder -> Builder
verMsg version time nonce scanBlocks = 
  msg msgType msgSize $ word32BE version
                     <> word64BE time
                     <> word64BE nonce
                     <> word32BE currenciesAmount <> V.foldl (<>) mempty scanBlocks
  where
    msgType = 0
    scanBlockSize = 24
    currenciesAmount = fromIntegral $ V.length scanBlocks
    msgSize = genericSizeOf time + genericSizeOf nonce + currenciesAmount * scanBlockSize

verACKMsg :: Builder
verACKMsg = msg msgType msgSize mempty
  where
    msgType = 1
    msgSize = 0

pingMsg :: Word64 -> Builder
pingMsg nonce = msg msgType msgSize $ word64BE nonce
  where
    msgType = genericSizeOf nonce
    msgSize = 4

pongMsg :: Word64 -> Builder
pongMsg nonce = msg msgType msgSize $ word64BE nonce
  where
    msgType = 12
    msgSize = genericSizeOf nonce

msg :: Word32 -> Word32 -> Builder -> Builder
msg msgType msgLength payload = word32BE msgType <> word32BE msgLength <> payload 