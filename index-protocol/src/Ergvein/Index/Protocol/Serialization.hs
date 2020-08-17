module Ergvein.Index.Protocol.Serialization where

import Codec.Compression.GZip
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Foreign.C.Types

import Ergvein.Index.Protocol.Types
import Ergvein.Types.Fees

import qualified Data.ByteString as BS
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

messageBase :: MessageType -> Word32 -> Builder -> Builder
messageBase msgType msgLength payload = word32BE (messageTypeToWord32 msgType) <> word32BE msgLength <> payload

scanBlockBuilder :: ScanBlock -> (Sum Word32, Builder)
scanBlockBuilder ScanBlock {..} = (scanBlockSize, scanBlock)
  where
    currencyCode = currencyCodeToWord32 scanBlockCurrency
    scanBlockSize = Sum $ genericSizeOf currencyCode
                        + genericSizeOf scanBlockVersion
                        + genericSizeOf scanBlockScanHeight
                        + genericSizeOf scanBlockHeight

    scanBlock = word32BE currencyCode
             <> word32BE scanBlockVersion
             <> word64BE scanBlockScanHeight
             <> word64BE scanBlockHeight

blockFilterBuilder :: BlockFilter -> (Sum Word32, Builder)
blockFilterBuilder BlockFilter {..} = (filterSize, filterBuilder)
  where
    idLength = fromIntegral $ BS.length blockFilterBlockId
    filterLength = fromIntegral $ BS.length blockFilterFilter
    filterSize = Sum $ genericSizeOf idLength
                     + idLength
                     + genericSizeOf filterLength
                     + filterLength
    filterBuilder = word32BE idLength
                 <> byteString blockFilterBlockId
                 <> word32BE filterLength
                 <> byteString blockFilterFilter

feeLevelToWord8 :: FeeLevel -> Word8
feeLevelToWord8 fl = case fl of
  FeeFast     -> 0
  FeeModerate -> 1
  FeeCheap    -> 2

messageBuilder :: Message -> Builder

messageBuilder (MPing msg) = messageBase MPingType msgSize $ word64BE msg
  where
    msgSize = genericSizeOf msg

messageBuilder (MPong msg) = messageBase MPongType msgSize $ word64BE msg
  where
    msgSize = genericSizeOf msg

messageBuilder (MReject msg) = messageBase MRejectType msgSize $ word32BE rejectType
  where
    rejectType = rejectTypeToWord32 $ rejectMsgCode msg
    msgSize = genericSizeOf rejectType

messageBuilder (MVersionACK msg) = messageBase MVersionACKType msgSize $ word16BE 0
  where
    msgSize = genericSizeOf (0 :: Word16)

messageBuilder (MVersion Version {..}) =
  messageBase MVersionType msgSize
  $  word32BE versionVersion
  <> word64BE (fromIntegral time)
  <> word64BE versionNonce
  <> word32BE scanBlocksCount
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
  $  word32BE currency
  <> word64BE filterRequestMsgStart
  <> word64BE filterRequestMsgAmount
  where
    currency = currencyCodeToWord32 filterRequestMsgCurrency

    msgSize = genericSizeOf currency
            + genericSizeOf filterRequestMsgStart
            + genericSizeOf filterRequestMsgAmount

messageBuilder (MFiltersResponse FilterResponse {..}) =
  messageBase MFiltersResponseType msgSize
  $  word32BE (currencyCodeToWord32 filterResponseCurrency)
  <> word32BE filtersCount
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
  $  word32BE currency
  <> word64BE filterEventHeight
  <> word32BE filterEventBlockIdLength
  <> byteString filterEventBlockId
  <> word32BE filterEventBlockFilterLength
  <> byteString filterEventBlockFilter
  where
    currency = currencyCodeToWord32 filterEventCurrency
    filterEventBlockIdLength = fromIntegral $ BS.length filterEventBlockId
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
  cursBS = mconcat $ (word32BE . currencyCodeToWord32) <$> curs
  msg = word32BE amount <> cursBS
  in messageBase MFeeRequestType msgSize msg

messageBuilder (MFeeResponse msgs) = let
  amount = fromIntegral $ length msgs
  (respSum, resps) = mconcat $ feeRespBuilder <$> msgs
  msgSize = genericSizeOf amount + (getSum respSum)
  msg = word32BE amount <> resps
  in messageBase MFeeResponseType msgSize msg

feeRespBuilder :: FeeResp -> (Sum Word32, Builder)
feeRespBuilder (FeeRespBTC isTest (FeeBundle (a,b) (c,d) (e,f))) = let
  cur = currencyCodeToWord32 $ if isTest then TBTC else BTC
  msgSize = genericSizeOf cur + 6 * genericSizeOf a
  msg = word32BE cur <> word64BE a <> word64BE b <> word64BE c <> word64BE d <> word64BE e <> word64BE f
  in (Sum msgSize, msg)

feeRespBuilder (FeeRespGeneric cur h m l) = let
  currency = currencyCodeToWord32 cur
  msgSize = genericSizeOf currency + 3 * genericSizeOf h
  msg = word32BE currency <> word64BE h <> word64BE m <> word64BE l
  in (Sum msgSize, msg)
