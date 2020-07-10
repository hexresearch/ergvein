module Ergvein.Index.Protocol.Types where

import Data.Word

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