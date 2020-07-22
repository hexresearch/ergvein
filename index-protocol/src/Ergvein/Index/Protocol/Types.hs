module Ergvein.Index.Protocol.Types where

import Data.Time.Clock.POSIX
import Data.Vector.Unboxed.Deriving
import Data.Word
import Foreign.Storable
import Language.Haskell.TH
import qualified Data.Vector.Unboxed as V

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

data CurrencyCode = BTC   | TBTC
                  | ERGO  | TERGO
                  | USDTO | TUSDTO
                  | LTC   | TLTC
                  | ZEC   | TZEC
                  | CPR   | TCPR
                  | DASH  | TDASH

currencyCodeToWord32 = \case
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

word32ToCurrencyCode = \case
  0  -> BTC
  1  -> TBTC
  2  -> ERGO
  3  -> TERGO
  4  -> USDTO
  5  -> TUSDTO
  6  -> LTC
  7  -> TLTC
  8  -> ZEC
  9  -> TZEC
  10 -> CPR
  11 -> TCPR
  12 -> DASH
  13 -> TDASH

derivingUnbox "CurrencyCode"
  [t| CurrencyCode -> Word32 |]
  [| currencyCodeToWord32    |]
  [| word32ToCurrencyCode    |]

data MessageHeader = MessageHeader
  { msgType :: MessageType
  , msgSize :: Word32
  }

type PingMessage = Word64

type PongMessage = Word64

data RejectMessage = RejectMessage
  { rejectMsgCode :: RejectCode
  }

data ScanBlock = ScanBlock
  { scanBlockCurrency   :: CurrencyCode
  , scanBlockVersion    :: Word32
  , scanBlockScanHeight :: Word64
  , scanBlockHeight     :: Word64
  }

derivingUnbox "ScanBlock"
  [t| ScanBlock -> (CurrencyCode, Word32, Word64, Word64) |]
  [| \(ScanBlock c v s h) -> (c, v, s, h) |]
  [| \(c, v, s, h) -> ScanBlock c v s h |]

data VersionMessage = VersionMessage
  { versionMsgVersion    :: Word32
  , versionMsgTime       :: POSIXTime
  , versionMsgNonce      :: Word64
  , versionMsgCurrencies :: Word32
  , versionMsgScanBlocks :: V.Vector ScanBlock
  }
  

data VersionACKMessage = VersionACKMessage

data FilterRequestMessage = FilterRequestMessage
  { filterRequestMsgCurrency :: CurrencyCode
  , filterRequestMsgStart    :: Word64
  , filterRequestMsgAmount   :: Word64
  }

data Message = PingMsg       PingMessage
             | PongMsg       PongMessage
             | VersionMsg    VersionMessage
             | VersionACKMsg VersionACKMessage
             | RejectMsg     RejectMessage

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf