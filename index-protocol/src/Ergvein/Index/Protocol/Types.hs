module Ergvein.Index.Protocol.Types where

import Conversion
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Fixed
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector.Unboxed.Deriving
import Data.Word
import Ergvein.Text
import Foreign.C.Types
import Foreign.Storable
import Network.Socket (SockAddr(..))

import Ergvein.Types.Fees
import Ergvein.Types.Currency (Fiat)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Ergvein.Types.Currency as E

-- | Protocol version that follows semantic version (major, minor, patch),
-- where minor adds backward compatible features and patch refers to bug fixes.
--
-- In memory it is encoded as LE word32: 2 bits reserved + 10 bits major + 10 bits minor + 10 bits patch
-- LE order means that first byte in memory contains patch component.
type ProtocolVersion = (Word16, Word16, Word16)

protocolVersion :: ProtocolVersion
protocolVersion = (2,0,0)

showProtocolVersion :: ProtocolVersion -> Text
showProtocolVersion (a,b,c) = showt a <> "." <> showt b <> "." <> showt c

-- | Compare own version with other version and check whether we support it
isCompatible :: ProtocolVersion -> ProtocolVersion -> Bool
isCompatible (1,_, _) (0, 0, 4) = True -- Pre 1.0.0 versions encoded as simple LE number. Thus 1 => 0b00000001000000000000000000000000 => 0b100 patch version (10 bits patch version in LE)
isCompatible (major1, _, _) (major2, _, _) = major1 == major2

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
                 | MRatesRequestType
                 | MRatesResponseType
  deriving (Eq, Ord, Enum, Bounded, Show)

messageHasPayload :: MessageType -> Bool 
messageHasPayload = \case
  MVersionACKType -> False
  MPeerRequestType -> False
  _ -> True

data RejectCode = MessageHeaderParsing | MessageParsing | InternalServerError | ZeroBytesReceived | VersionNotSupported
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

codeToCurrency :: CurrencyCode -> Maybe E.Currency
codeToCurrency = \case
  BTC    -> Just E.BTC
  TBTC   -> Just E.BTC
  ERGO   -> Just E.ERGO
  TERGO  -> Just E.ERGO
  _ -> Nothing

currencyToCode :: Bool -> E.Currency -> CurrencyCode
currencyToCode test = \case
  E.BTC -> if test then TBTC else BTC
  E.ERGO -> if test then TERGO else ERGO

currencyCodeTestnet :: CurrencyCode -> Bool
currencyCodeTestnet = \case
  BTC    -> False
  TBTC   -> True
  ERGO   -> False
  TERGO  -> True
  USDTO  -> False
  TUSDTO -> True
  LTC    -> False
  TLTC   -> True
  ZEC    -> False
  TZEC   -> True
  CPR    -> False
  TCPR   -> True
  DASH   -> False
  TDASH  -> True

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
  { rejectId      :: !MessageType
  , rejectMsgCode :: !RejectCode
  , rejectMsg     :: !Text
  } deriving (Show, Eq)

data ScanBlock = ScanBlock
  { scanBlockCurrency   :: !CurrencyCode
  , scanBlockVersion    :: !ProtocolVersion
  , scanBlockScanHeight :: !Word64
  , scanBlockHeight     :: !Word64
  } deriving (Show, Eq)

derivingUnbox "ScanBlock"
  [t| ScanBlock -> (CurrencyCode, Word16, Word16, Word16, Word64, Word64) |]
  [| \(ScanBlock c (v1, v2, v3) s h) -> (c, v1, v2, v3, s, h) |]
  [| \(c, v1, v2, v3, s, h) -> ScanBlock c (v1, v2, v3) s h |]

data Version = Version
  { versionVersion    :: !ProtocolVersion
  , versionTime       :: !CTime
  , versionNonce      :: !Word64
 -- versionCurrencies :: uint32 Amount of currencies blocks following the field. For clients it is 0.
  , versionScanBlocks :: !(UV.Vector ScanBlock)
  } deriving (Show, Eq)

versionCurrencies :: Version -> [CurrencyCode]
versionCurrencies Version{..} = nub . fmap scanBlockCurrency . UV.toList $ versionScanBlocks

versionHasCurr :: Version -> CurrencyCode -> Bool
versionHasCurr Version{..} c = UV.any ((c ==) . scanBlockCurrency) versionScanBlocks

versionHasCurrs :: Foldable t => Version -> t CurrencyCode -> Bool
versionHasCurrs v = all (versionHasCurr v)

versionCurrSynced :: Version -> CurrencyCode -> Bool
versionCurrSynced Version{..} c = flip UV.any versionScanBlocks $ \ScanBlock{..} ->
     scanBlockCurrency == c
  && (scanBlockHeight - scanBlockScanHeight) <= 1

versionCurrsSynced :: Foldable t => Version -> t CurrencyCode -> Bool
versionCurrsSynced v = all (versionCurrSynced v)

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

data IPType = IPV4 | IPV6 | OnionV3
  deriving (Eq, Ord, Enum, Bounded, Show)

ipTypeToWord8 :: IPType -> Word8
ipTypeToWord8 = \case
  IPV4 -> 0
  IPV6 -> 1
  OnionV3 -> 2

word8ToIPType :: Word8 -> Maybe IPType
word8ToIPType = \case
  0 -> Just IPV4
  1 -> Just IPV6
  2 -> Just OnionV3
  _ -> Nothing

addressSize :: IPType -> Word32
addressSize = \case
  IPV4 -> 4
  IPV6 -> 16
  OnionV3 -> 56

addressType :: Address -> IPType
addressType = \case
  AddressIpv4{} -> IPV4
  AddressIpv6{} -> IPV6
  AddressOnionV3{} -> OnionV3

data IpV6 = IpV6 !Word32 !Word32 !Word32 !Word32
  deriving (Show, Eq)

data Address
  = AddressIpv4 {
      addressV4      :: !Word32
    , addressPort    :: !Word16
    }
  | AddressIpv6 {
      addressV6      :: !IpV6
    , addressPort    :: !Word16
    }
  | AddressOnionV3 {
      addressOnion   :: !ByteString
    , addressPort    :: !Word16
    }
  deriving (Show, Eq)

data PeerRequest = PeerRequest
  deriving (Show, Eq)

data PeerResponse = PeerResponse
  { peerResponseAddresses :: !(V.Vector Address)
  } deriving (Show, Eq)

data PeerIntroduce = PeerIntroduce
  { peerIntroduceAddresses :: !(V.Vector Address)
  } deriving (Show, Eq)

newtype RatesRequest = RatesRequest { unRatesRequest :: Map CurrencyCode [Fiat] }
  deriving (Show, Eq)

newtype RatesResponse = RatesResponse { unRatesResponse :: Map CurrencyCode (Map Fiat Centi)}
  deriving (Show, Eq)

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
             | MRatesRequest               !RatesRequest
             | MRatesResponse              !RatesResponse
  deriving (Show, Eq)

genericSizeOf :: (Storable a, Integral b) => a -> b
genericSizeOf = fromIntegral . sizeOf

instance Conversion Address SockAddr where
  convert addr = case addr of
    AddressIpv4 {..} -> let
      port = fromInteger $ toInteger addressPort
      in SockAddrInet port addressV4
    AddressIpv6 (IpV6 a b c d) p -> let
      port = fromInteger $ toInteger p
      ip  =  (a, b, c, d)
      in SockAddrInet6 port 0 ip 0
    AddressOnionV3{} -> error "Cannot convert onion address to socket address!"

messageType :: Message -> MessageType
messageType = \case
  MPing{} -> MPingType
  MPong{} -> MPongType
  MVersion{} -> MVersionType
  MVersionACK{} -> MVersionACKType
  MReject{} -> MRejectType
  MFiltersRequest{} -> MFiltersRequestType
  MFiltersResponse{} -> MFiltersResponseType
  MFiltersEvent{} -> MFilterEventType
  MFeeRequest{} -> MFeeRequestType
  MFeeResponse{} -> MFeeResponseType
  MPeerRequest{} -> MPeerRequestType
  MPeerResponse{} -> MPeerResponseType
  MPeerIntroduce{} -> MIntroducePeerType
  MRatesRequest{} -> MRatesRequestType
  MRatesResponse{} -> MRatesResponseType
