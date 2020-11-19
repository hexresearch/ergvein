module Ergvein.Index.Protocol.Types where

import Data.ByteString
import Data.ByteString.Short (ShortByteString)
import Data.Vector.Unboxed.Deriving
import Data.Word
import Foreign.C.Types
import Foreign.Storable

import Ergvein.Types.Fees

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

protocolVersion :: Word32
protocolVersion = 1

data MessageType = MVersionType
                 | MVersionACKType
                 | MFiltersRequestType
                 | MFiltersResponseType
                 | MFilterEventType
                 | MPeerRequestType
                 | MPeerResponseType
                 | MFeeRequestType
                 | MFeeResponseType
                 | MIntroducePeerType
                 | MRejectType
                 | MPingType
                 | MPongType
  deriving (Eq, Ord, Enum, Bounded, Show)

data RejectCode = MessageHeaderParsing | MessageParsing | InternalServerError | ZeroBytesReceived
  deriving (Eq, Ord, Enum, Bounded, Show)

data CurrencyCode = BTC   | TBTC
                  | ERGO  | TERGO
                  | USDTO | TUSDTO
                  | LTC   | TLTC
                  | ZEC   | TZEC
                  | CPR   | TCPR
                  | DASH  | TDASH
  deriving (Eq, Ord, Enum, Bounded, Show)

currencyCodeToWord32 :: CurrencyCode -> Word32
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

word32ToCurrencyCode :: Word32 -> Maybe CurrencyCode
word32ToCurrencyCode = \case
  0  -> Just BTC
  1  -> Just TBTC
  2  -> Just ERGO
  3  -> Just TERGO
  4  -> Just USDTO
  5  -> Just TUSDTO
  6  -> Just LTC
  7  -> Just TLTC
  8  -> Just ZEC
  9  -> Just TZEC
  10 -> Just CPR
  11 -> Just TCPR
  12 -> Just DASH
  13 -> Just TDASH
  _  -> Nothing

derivingUnbox "CurrencyCode"
  [t| CurrencyCode -> Word8  |]
  [| fromIntegral . fromEnum |]
  [| toEnum . fromIntegral   |]

data MessageHeader = MessageHeader
  { msgType :: !MessageType
  , msgSize :: !Word32
  } deriving (Show, Eq)

type Ping = Word64

type Pong = Word64

data Reject = Reject
  { rejectMsgCode :: !RejectCode
  } deriving (Show, Eq)

data ScanBlock = ScanBlock
  { scanBlockCurrency   :: !CurrencyCode
  , scanBlockVersion    :: !Word32
  , scanBlockScanHeight :: !Word64
  , scanBlockHeight     :: !Word64
  } deriving (Show, Eq)

derivingUnbox "ScanBlock"
  [t| ScanBlock -> (CurrencyCode, Word32, Word64, Word64) |]
  [| \(ScanBlock c v s h) -> (c, v, s, h) |]
  [| \(c, v, s, h) -> ScanBlock c v s h |]

data Version = Version
  { versionVersion    :: !Word32
  , versionTime       :: !CTime
  , versionNonce      :: !Word64
 -- versionCurrencies :: uint32 Amount of currencies blocks following the field. For clients it is 0.
  , versionScanBlocks :: !(UV.Vector ScanBlock)
  } deriving (Show, Eq)

data VersionACK = VersionACK
  deriving (Show, Eq)

data FilterRequest = FilterRequest
  { filterRequestMsgCurrency :: !CurrencyCode
  , filterRequestMsgStart    :: !Word64
  , filterRequestMsgAmount   :: !Word64
  } deriving (Show, Eq)

data BlockFilter = BlockFilter
  { -- blockFilterBlockIdLength :: uint32 Length of block hash
    blockFilterBlockId       :: !ShortByteString
 -- blockFilterLength :: uint32 Size in bytes of filter
  , blockFilterFilter        :: !ByteString
  } deriving (Show, Eq)

data FilterResponse = FilterResponse
  { filterResponseCurrency :: !CurrencyCode
  , filterResponseFilters  :: !(V.Vector BlockFilter)
  } deriving (Show, Eq)

data FilterEvent = FilterEvent
  { filterEventCurrency      :: !CurrencyCode
  , filterEventHeight        :: !Word64
 -- filterEventBlockIdLength :: uint32 Length of block hash
  , filterEventBlockId       :: !ShortByteString
 -- filterEventFilterLength  :: uint32 Length of filter
  , filterEventBlockFilter   :: !ByteString
  } deriving (Show, Eq)

data FeeResp
  = FeeRespBTC !Bool !FeeBundle         -- Bool -- isTestnet
  | FeeRespGeneric !CurrencyCode !Word64 !Word64 !Word64
  deriving (Show, Eq)

type FeeResponse = [FeeResp]
type FeeRequest = [CurrencyCode]

data IPType = IPV4 | IPV6
  deriving (Eq, Ord, Enum, Bounded, Show)


ipTypeToWord8 :: IPType -> Word8
ipTypeToWord8 = \case
  IPV4 -> 0
  IPV6 -> 1

word8ToIPType :: Word8 -> IPType
word8ToIPType = \case
  0 -> IPV4
  1 -> IPV6

data Address = Address
  { addressType    :: !IPType
  , addressPort    :: !Word16
  , addressAddress :: !ByteString
  } deriving (Show, Eq)

data PeerRequest = PeerRequest
  deriving (Show, Eq)

data PeerResponse = PeerResponse
  { peerResponseAddresses :: !(V.Vector Address)
  } deriving (Show, Eq)

data PeerIntroduce = PeerIntroduce
  { peerIntroduceAddresses :: !(V.Vector Address)
  } deriving (Show, Eq)

data Message = MPing                       !Ping
             | MPong                       !Pong
             | MVersion                    !Version
             | MVersionACK                 !VersionACK
             | MReject                     !Reject
             | MFiltersRequest             !FilterRequest
             | MFiltersResponse            !FilterResponse
             | MFiltersEvent               !FilterEvent
             | MFeeRequest                 !FeeRequest
             | MFeeResponse                !FeeResponse
             | MPeerRequest                !PeerRequest
             | MPeerResponse               !PeerResponse
             | MPeerIntroduce              !PeerIntroduce
  deriving (Show, Eq)

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf
