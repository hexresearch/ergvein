module Ergvein.Index.Protocol.Types where

import Data.Word
import qualified Data.Vector as V
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

data RejectCode = MessageHeaderParsing | MessageParsing | InternalServerError

data CurrencyCode = BTC | TBTC | ERGO | TERGO | USDTO | TUSDTO | LTC | TLTC | ZEC | TZEC | CPR | TCPR | DASH | TDASH

data MessageHeader = MessageHeader
  { msgType :: MessageType
  , msgSize :: Word32
  }

type PingMessage = Word64

type PongMessage = Word64

data RejectMessage = RejectMessage
  { rejectMsgCode :: RejectCode
  }

data Message = PingMsg   PingMessage
             | PongMsg   PongMessage
             | RejectMsg RejectMessage

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf