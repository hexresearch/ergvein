module Ergvein.Index.Protocol.Serialization where

import Codec.Compression.GZip
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Foreign.C.Types

import Ergvein.Index.Protocol.Types
import Ergvein.Types.Fees

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as LBS
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

rejectTypeToWord32 :: RejectCode -> Word32
rejectTypeToWord32 = \case
  MessageHeaderParsing -> 0
  MessageParsing       -> 1
  InternalServerError  -> 2

feeLevelToWord8 :: FeeLevel -> Word8
feeLevelToWord8 fl = case fl of
  FeeFast     -> 0
  FeeModerate -> 1
  FeeCheap    -> 2

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

messageBuilder (MVersionACK msg) = messageBase MVersionACKType msgSize $ word8 msg
  where
    msg = 0 :: Word8
    msgSize = genericSizeOf msg

messageBuilder (MVersion Version {..}) =
  messageBase MVersionType msgSize
  $  word32LE versionVersion
  <> word64LE (fromIntegral time)
  <> word64LE versionNonce
  <> word32LE scanBlocksCount
  <> scanBlocks
  where
    (scanBlocksSizeSum, scanBlocks) = mconcat $ scanBlockBuilder <$> UV.toList versionScanBlocks
    scanBlocksCount = fromIntegral $ UV.length versionScanBlocks
    scanBlocksSize = getSum scanBlocksSizeSum
    msgSize = genericSizeOf versionVersion
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
    (filtersSizeSum, filters) = mconcat $ blockFilterBuilder <$> V.toList filterResponseFilters
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

messageBuilder (MPeerRequest _) = messageBase MIntroducePeerType msgSize $ word8 msg
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
  in messageBase MPeerResponseType msgSize
  $  word32LE addrAmount
  <> addresses

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
