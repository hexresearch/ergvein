module Ergvein.Index.Protocol.Serialization where

import Codec.Compression.GZip
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Data.Scientific
import Foreign.C.Types

import Ergvein.Index.Protocol.Types
import Ergvein.Types.Fees
import Ergvein.Types.Currency (Fiat)

import qualified Data.Bitstream as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

messageTypeToWord32 :: MessageType -> Word32
messageTypeToWord32 = \case
  MVersionType         -> 0
  MVersionACKType      -> 1
  MFiltersRequestType  -> 2
  MFiltersResponseType -> 3
  MFilterEventType     -> 4
  MPeerRequestType     -> 5
  MPeerResponseType    -> 6
  MFeeRequestType      -> 7
  MFeeResponseType     -> 8
  MIntroducePeerType   -> 9
  MRejectType          -> 10
  MPingType            -> 11
  MPongType            -> 12
  MRatesRequestType    -> 13
  MRatesResponseType   -> 14

rejectTypeToWord32 :: RejectCode -> Word32
rejectTypeToWord32 = \case
  MessageHeaderParsing -> 0
  MessageParsing       -> 1
  InternalServerError  -> 2
  ZeroBytesReceived    -> 3

feeLevelToWord8 :: FeeLevel -> Word8
feeLevelToWord8 fl = case fl of
  FeeFast     -> 0
  FeeModerate -> 1
  FeeCheap    -> 2

mkProtocolVersion :: ProtocolVersion -> BS.ByteString
mkProtocolVersion (mj,mn,p)
  | mj > 1023 = error $ "Major version out of bounds: " <> show mj <> " should be < 1023"
  | mn > 1023 = error $ "Minor version out of bounds: " <> show mn <> " should be < 1023"
  | p  > 1023 = error $ "Patch version out of bounds: " <> show p  <> " should be < 1023"
  | S.length protocolReservedBits /= (2 :: Int) = error "There should be only two reserved bits"
  | otherwise = S.toByteString $ protocolReservedBits <> w16to10 mj <> w16to10 mn <> w16to10 p
  where
    w16to10 :: Word16 -> S.Bitstream (S.Right)
    w16to10 = S.fromNBits (10 :: Int)

protocolVersionBS :: BS.ByteString
protocolVersionBS = mkProtocolVersion protocolVersion

addressBuilder :: Address -> (Sum Word32, Builder)
addressBuilder Address {..} = (addrSize, addrBuilder)
  where
    addrType = ipTypeToWord8 addressType
    addrSize = Sum $ genericSizeOf addrType
                   + genericSizeOf addressPort
                   + (if addressType == IPV4 then 4 else 16)
    addrBuilder = word8 addrType
               <> word16LE addressPort
               <> byteString addressAddress

messageBase :: MessageType -> Word32 -> Builder -> Builder
messageBase msgType msgLength payload = word32LE (messageTypeToWord32 msgType) <> word32LE msgLength <> payload

scanBlockBuilder :: ScanBlock -> (Sum Word32, Builder)
scanBlockBuilder ScanBlock {..} = (scanBlockSize, scanBlock)
  where
    currencyCode = currencyCodeToWord32 scanBlockCurrency
    scanBlockSize = Sum $ genericSizeOf currencyCode
                        + genericSizeOf scanBlockVersion
                        + genericSizeOf scanBlockScanHeight
                        + genericSizeOf scanBlockHeight

    scanBlock = word32LE currencyCode
             <> word32LE scanBlockVersion
             <> word64LE scanBlockScanHeight
             <> word64LE scanBlockHeight

blockFilterBuilder :: BlockFilter -> (Sum Word32, Builder)
blockFilterBuilder BlockFilter {..} = (filterSize, filterBuilder)
  where
    idLength = fromIntegral $ BSS.length blockFilterBlockId
    filterLength = fromIntegral $ BS.length blockFilterFilter
    filterSize = Sum $ genericSizeOf idLength
                     + idLength
                     + genericSizeOf filterLength
                     + filterLength
    filterBuilder = word32LE idLength
                 <> byteString (BSS.fromShort blockFilterBlockId)
                 <> word32LE filterLength
                 <> byteString blockFilterFilter

messageBuilder :: Message -> Builder

messageBuilder (MPing msg) = messageBase MPingType msgSize $ word64LE msg
  where
    msgSize = genericSizeOf msg

messageBuilder (MPong msg) = messageBase MPongType msgSize $ word64LE msg
  where
    msgSize = genericSizeOf msg

messageBuilder (MReject msg) = messageBase MRejectType msgSize $ word32LE rejectType
  where
    rejectType = rejectTypeToWord32 $ rejectMsgCode msg
    msgSize = genericSizeOf rejectType

messageBuilder (MVersionACK VersionACK) = messageBase MVersionACKType msgSize $ word8 msg
  where
    msg = 0 :: Word8
    msgSize = genericSizeOf msg

messageBuilder (MVersion Version {..}) =
  messageBase MVersionType msgSize
  $  byteString (mkProtocolVersion versionVersion)
  <> word64LE (fromIntegral time)
  <> word64LE versionNonce
  <> word32LE scanBlocksCount
  <> scanBlocks
  where
    (scanBlocksSizeSum, scanBlocks) = mconcat $ scanBlockBuilder <$> UV.toList versionScanBlocks
    scanBlocksCount = fromIntegral $ UV.length versionScanBlocks
    scanBlocksSize = getSum scanBlocksSizeSum
    msgSize = 4 -- Version is 4-byte long byteString
            + genericSizeOf versionTime
            + genericSizeOf versionNonce
            + genericSizeOf scanBlocksCount
            + scanBlocksSize
    (CTime time) = versionTime

messageBuilder (MFiltersRequest FilterRequest {..}) =
  messageBase MFiltersRequestType msgSize
  $  word32LE currency
  <> word64LE filterRequestMsgStart
  <> word64LE filterRequestMsgAmount
  where
    currency = currencyCodeToWord32 filterRequestMsgCurrency

    msgSize = genericSizeOf currency
            + genericSizeOf filterRequestMsgStart
            + genericSizeOf filterRequestMsgAmount

messageBuilder (MFiltersResponse FilterResponse {..}) =
  messageBase MFiltersResponseType msgSize
  $  word32LE (currencyCodeToWord32 filterResponseCurrency)
  <> word32LE filtersCount
  <> lazyByteString zippedFilters
  where
    (_filtersSizeSum, filters) = mconcat $ blockFilterBuilder <$> V.toList filterResponseFilters
    filtersCount = fromIntegral $ V.length filterResponseFilters
    zippedFilters = compress $ toLazyByteString filters

    msgSize = genericSizeOf (currencyCodeToWord32 filterResponseCurrency)
            + genericSizeOf filtersCount
            + fromIntegral (LBS.length zippedFilters)

messageBuilder (MFiltersEvent FilterEvent {..}) =
  messageBase MFilterEventType msgSize
  $  word32LE currency
  <> word64LE filterEventHeight
  <> word32LE filterEventBlockIdLength
  <> byteString (BSS.fromShort filterEventBlockId)
  <> word32LE filterEventBlockFilterLength
  <> byteString filterEventBlockFilter
  where
    currency = currencyCodeToWord32 filterEventCurrency
    filterEventBlockIdLength = fromIntegral $ BSS.length filterEventBlockId
    filterEventBlockFilterLength = fromIntegral $ BS.length filterEventBlockFilter

    msgSize = genericSizeOf currency
            + genericSizeOf filterEventHeight
            + genericSizeOf filterEventBlockIdLength
            + filterEventBlockIdLength
            + genericSizeOf filterEventBlockFilterLength
            + filterEventBlockFilterLength

messageBuilder (MFeeRequest curs) = let
  amount = fromIntegral $ length curs
  msgSize = case curs of
    [] -> genericSizeOf amount
    c:_ -> genericSizeOf amount + amount * genericSizeOf (currencyCodeToWord32 c)
  cursBS = mconcat $ (word32LE . currencyCodeToWord32) <$> curs
  msg = word32LE amount <> cursBS
  in messageBase MFeeRequestType msgSize msg

messageBuilder (MFeeResponse msgs) = let
  amount = fromIntegral $ length msgs
  (respSum, resps) = mconcat $ feeRespBuilder <$> msgs
  msgSize = genericSizeOf amount + getSum respSum
  msg = word32LE amount <> resps
  in messageBase MFeeResponseType msgSize msg

messageBuilder (MPeerRequest _) = messageBase MPeerRequestType msgSize $ word8 msg
  where
    msg = 0 :: Word8
    msgSize = genericSizeOf msg

messageBuilder (MPeerResponse PeerResponse{..}) = let
  (addressesSize, addresses) = mconcat $ addressBuilder <$> V.toList peerResponseAddresses
  addrAmount = fromIntegral $ V.length peerResponseAddresses
  msgSize = genericSizeOf addrAmount
          + getSum addressesSize
  in messageBase MPeerResponseType msgSize
  $  word32LE addrAmount
  <> addresses

messageBuilder (MPeerIntroduce PeerIntroduce{..}) = let
  (addressesSize, addresses) = mconcat $ addressBuilder <$> V.toList peerIntroduceAddresses
  addrAmount = fromIntegral $ V.length peerIntroduceAddresses
  msgSize = genericSizeOf addrAmount
          + getSum addressesSize
  in messageBase MIntroducePeerType msgSize
  $  word32LE addrAmount
  <> addresses

messageBuilder (MRatesRequest (RatesRequest rs)) = let
  rsNum = fromIntegral $ length rs
  (size, body) = mconcat $ fmap cfBuilder $ M.toList rs
  msgSize = genericSizeOf rsNum + (getSum size)
  in messageBase MRatesRequestType msgSize $ word32LE rsNum <> body
--
messageBuilder (MRatesResponse (RatesResponse rs)) = let
  rsNum = fromIntegral $ length rs
  (size, body) = mconcat $ fmap cfdBuilder $ M.toList rs
  msgSize = genericSizeOf rsNum + (getSum size)
  in messageBase MRatesResponseType msgSize $ word32LE rsNum <> body

enumBuilder :: Enum a => a -> Builder
enumBuilder = word32LE . fromIntegral . fromEnum

cfBuilder :: (CurrencyCode, [Fiat]) -> (Sum Word32, Builder)
cfBuilder (cc, fs) = let
  fsNum = fromIntegral $ length fs
  body = mconcat $ enumBuilder <$> fs
  size = Sum $ (fsNum + 2) * (genericSizeOf fsNum)
  in (size, ) $ enumBuilder cc <> word32LE fsNum <> body

cfdBuilder :: (CurrencyCode, M.Map Fiat Double) -> (Sum Word32, Builder)
cfdBuilder (cc, fds) = let
  fdsNum = fromIntegral $ length fds
  body = mconcat $ fmap fdBuilder $ M.toList fds
  size = Sum $ 8 + fdsNum * 20
  in (size, ) $ enumBuilder cc <> word32LE fdsNum <> body

-- | size 20
fdBuilder :: (Fiat, Double) -> Builder
fdBuilder (f, d) = enumBuilder f <> doubleBuilder d

-- | Build Double as two Word64. size = 16
doubleBuilder :: Double -> Builder
doubleBuilder v = let
  sci = normalize $ fromFloatDigits v
  c = fromIntegral $ coefficient sci
  e = fromIntegral $ base10Exponent sci
  in word64LE c <> word64LE e

feeRespBuilder :: FeeResp -> (Sum Word32, Builder)
feeRespBuilder (FeeRespBTC isTest (FeeBundle (a,b) (c,d) (e,f))) = let
  cur = currencyCodeToWord32 $ if isTest then TBTC else BTC
  msgSize = genericSizeOf cur + 6 * genericSizeOf a
  msg = word32LE cur <> word64LE a <> word64LE b <> word64LE c <> word64LE d <> word64LE e <> word64LE f
  in (Sum msgSize, msg)

feeRespBuilder (FeeRespGeneric cur h m l) = let
  currency = currencyCodeToWord32 cur
  msgSize = genericSizeOf currency + 3 * genericSizeOf h
  msg = word32LE currency <> word64LE h <> word64LE m <> word64LE l
  in (Sum msgSize, msg)
