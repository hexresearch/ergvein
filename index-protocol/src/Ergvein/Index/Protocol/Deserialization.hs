module Ergvein.Index.Protocol.Deserialization where

import Codec.Compression.GZip
import Control.Monad
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.List
import Data.Word

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Utils
import Ergvein.Types.Fees

import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

word32toMessageType :: Word32 -> Maybe MessageType
word32toMessageType = \case
  0  -> Just MVersionType
  1  -> Just MVersionACKType
  2  -> Just MFiltersRequestType
  3  -> Just MFiltersResponseType
  4  -> Just MFilterEventType
  5  -> Just MPeerRequestType
  6  -> Just MPeerResponseType
  7  -> Just MFeeRequestType
  8  -> Just MFeeResponseType
  9  -> Just MIntroducePeerType
  10 -> Just MRejectType
  11 -> Just MPingType
  12 -> Just MPongType
  _  -> Nothing

currencyCodeParser :: Parser CurrencyCode
currencyCodeParser = fmap word32ToCurrencyCode anyWord32be

word32toRejectType :: Word32 -> Maybe RejectCode
word32toRejectType = \case
  0  -> Just MessageHeaderParsing
  1  -> Just MessageParsing
  2  -> Just InternalServerError
  _  -> Nothing

word8toFeeLevel :: Word8 -> Maybe FeeLevel
word8toFeeLevel = \case
  0 -> Just FeeFast
  1 -> Just FeeModerate
  2 -> Just FeeCheap
  _ -> Nothing

messageHeaderParser ::  Parser MessageHeader
messageHeaderParser = do
    messageType <- messageTypeParser
    messageSize <- anyWord32be
    pure $ MessageHeader messageType messageSize

messageTypeParser :: Parser MessageType
messageTypeParser = guardJust "out of message type bounds" . word32toMessageType =<< anyWord32be

rejectCodeParser :: Parser RejectCode
rejectCodeParser = guardJust "out of reject type bounds" . word32toRejectType =<< anyWord32be

feeLevelParser :: Parser FeeLevel
feeLevelParser = guardJust "out of feeLevel type bounds" . word8toFeeLevel =<< anyWord8

versionBlockParser ::  Parser ScanBlock
versionBlockParser = do
  currency   <- currencyCodeParser
  version    <- anyWord32be
  scanHeight <- anyWord64be
  height     <- anyWord64be

  pure $ ScanBlock
    { scanBlockCurrency   = currency
    , scanBlockVersion    = version
    , scanBlockScanHeight = scanHeight
    , scanBlockHeight     = height
    }

parseFilters :: BS.ByteString -> [BlockFilter]
parseFilters = unfoldr (\source ->
  case parse filterParser source of
    Done rest parsedFilter -> Just (parsedFilter, rest)
    _ -> Nothing)

filterParser :: Parser BlockFilter
filterParser = do
  blockIdLength <- fromIntegral <$> anyWord32be
  blockId <- Parse.take blockIdLength
  blockFilterLength <- fromIntegral <$> anyWord32be
  blockFilter <- Parse.take blockFilterLength

  pure $ BlockFilter
    { blockFilterBlockId = blockId
    , blockFilterFilter  = blockFilter
    }

messageParser :: MessageType -> Parser Message
messageParser MPingType = MPing <$> anyWord64be

messageParser MPongType = MPong <$> anyWord64be

messageParser MRejectType = MReject . Reject <$> rejectCodeParser

messageParser MVersionType = do
  version       <- anyWord32be
  time          <- fromIntegral <$> anyWord64be
  nonce         <- anyWord64be
  currencies    <- anyWord32be
  versionBlocks <- UV.fromList <$> replicateM (fromIntegral currencies) versionBlockParser

  pure $ MVersion $ Version
    { versionVersion    = version
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = versionBlocks
    }

messageParser MVersionACKType = pure $ MVersionACK VersionACK

messageParser MFiltersRequestType = do
  currency <- currencyCodeParser
  start    <- anyWord64be
  amount   <- anyWord64be

  pure $ MFiltersRequest $ FilterRequest
    { filterRequestMsgCurrency = currency
    , filterRequestMsgStart    = start
    , filterRequestMsgAmount   = amount
    }

messageParser MFiltersResponseType = do
  currency <- currencyCodeParser
  amount <- anyWord32be
  filtersString <- takeLazyByteString

  let unzippedFilters = LBS.toStrict $ decompress filtersString
      parser = V.fromList <$> replicateM (fromIntegral amount) filterParser

  case parseOnly parser unzippedFilters of
    Right parsedFilters -> pure $ MFiltersResponse $ FilterResponse
      { filterResponseCurrency = currency
      , filterResponseFilters  = parsedFilters
      }
    _ -> fail "fail to parse response filters"


messageParser MFilterEventType = do
  currency <- currencyCodeParser
  height <- anyWord64be
  blockIdLength <- fromIntegral <$> anyWord32be
  blockId <- Parse.take blockIdLength
  blockFilterLength <- fromIntegral <$> anyWord32be
  blockFilter <- Parse.take blockFilterLength

  pure $ MFiltersEvent $ FilterEvent
    { filterEventCurrency    = currency
    , filterEventHeight      = height
    , filterEventBlockId     = blockId
    , filterEventBlockFilter = blockFilter
    }

messageParser MFeeRequestType = do
  amount <- anyWord32be
  curs <- replicateM (fromIntegral amount) currencyCodeParser
  pure $ MFeeRequest curs

messageParser MFeeResponseType = do
  amount <- anyWord32be
  resps <- replicateM (fromIntegral amount) parseFeeResp
  pure $ MFeeResponse resps

parseFeeResp :: Parser FeeResp
parseFeeResp = do
  currency <- currencyCodeParser
  case currency of
    BTC -> btcParser False
    TBTC -> btcParser True
    _ -> genericParser currency
  where
    btcParser isTest = do
      h <- (,) <$> anyWord64be <*> anyWord64be
      m <- (,) <$> anyWord64be <*> anyWord64be
      l <- (,) <$> anyWord64be <*> anyWord64be
      pure $ FeeRespBTC isTest $ FeeBundle h m l
    genericParser cur = FeeRespGeneric cur
      <$> anyWord64be
      <*> anyWord64be
      <*> anyWord64be

parseMessage :: MessageType -> BS.ByteString -> Either String (Message, BS.ByteString)
parseMessage msgType source =
  case parse (messageParser msgType) source of
    Done rest message -> Right (message, rest)
    Partial _ -> Left "source too short"
    Fail _ _ err -> Left err
