module Ergvein.Index.Protocol.Types where

import Data.Word

import Foreign.Storable

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

data CurrencyCode = BTC | TBTC | ERGO | TERGO | USDTO | TUSDTO | LTC | TLTC | ZEC | TZEC | CPR | TCPR | DASH | TDASH

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf

data MessageHeader = MessageHeader
  { msgType :: MessageType
  , msgSize :: Word32
  }

type PingMessage = Word64

type PongMessage = Word64

data Message = PingMessage PingMessage
             | PongMessage PongMessage