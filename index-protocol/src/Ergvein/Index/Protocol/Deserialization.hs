module Ergvein.Index.Protocol.Deserialization where

import Codec.Compression.GZip
import Control.Monad
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Scientific
import Data.Word

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Utils
import Ergvein.Types.Fees
import Ergvein.Types.Currency (Fiat)

import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.Bitstream as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as M
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
  13 -> Just MRatesRequestType
  14 -> Just MRatesResponseType
  _  -> Nothing

currencyCodeParser :: Parser CurrencyCode
currencyCodeParser = do
  w <- anyWord32le
  case word32ToCurrencyCode w of
    Nothing -> fail "Invalid currency code"
    Just c  -> pure c

word32toRejectType :: Word32 -> Maybe RejectCode
word32toRejectType = \case
  0  -> Just MessageHeaderParsing
  1  -> Just MessageParsing
  2  -> Just InternalServerError
  3  -> Just ZeroBytesReceived
  _  -> Nothing

word8toFeeLevel :: Word8 -> Maybe FeeLevel
word8toFeeLevel = \case
  0 -> Just FeeFast
  1 -> Just FeeModerate
  2 -> Just FeeCheap
  _ -> Nothing

versionParser :: Parser ProtocolVersion
versionParser = do
  bs :: S.Bitstream (S.Right) <- S.fromBits <$> anyWord32be
  let rst  = S.drop i2 bs
  let mj   = S.toBits $ S.append pad $ S.take i10 rst
  let mn   = S.toBits $ S.append pad $ S.take i10 $ S.drop i10 rst
  let p    = S.toBits $ S.append pad $ S.take i10 $ S.drop i20 rst
  pure (mj,mn,p)
  where
    i2,i6,i10,i20 :: Int
    i2 = 2; i6 = 6 ; i10 = 10 ; i20 = 20
    pad = S.replicate i6 False

messageHeaderParser ::  Parser MessageHeader
messageHeaderParser = do
    messageType <- messageTypeParser
    messageSize <- anyWord32le
    pure $ MessageHeader messageType messageSize

messageTypeParser :: Parser MessageType
messageTypeParser = guardJust "out of message type bounds" . word32toMessageType =<< anyWord32le

rejectCodeParser :: Parser RejectCode
rejectCodeParser = guardJust "out of reject type bounds" . word32toRejectType =<< anyWord32le

feeLevelParser :: Parser FeeLevel
feeLevelParser = guardJust "out of feeLevel type bounds" . word8toFeeLevel =<< anyWord8

versionBlockParser ::  Parser ScanBlock
versionBlockParser = do
  currency   <- currencyCodeParser
  version    <- anyWord32le
  scanHeight <- anyWord64le
  height     <- anyWord64le

  pure $ ScanBlock
    { scanBlockCurrency   = currency
    , scanBlockVersion    = version
    , scanBlockScanHeight = scanHeight
    , scanBlockHeight     = height
    }

filterParser :: Parser BlockFilter
filterParser = do
  blockIdLength <- fromIntegral <$> anyWord32le
  blockId <- Parse.take blockIdLength
  blockFilterLength <- fromIntegral <$> anyWord32le
  blockFilter <- Parse.take blockFilterLength

  pure $ BlockFilter
    { blockFilterBlockId = BSS.toShort blockId
    , blockFilterFilter  = blockFilter
    }

addressParser :: Parser Address
addressParser = do
  addrType <-  maybe (fail "Invalid address type") pure
            .  word8ToIPType
           =<< anyWord8
  addrPort <- anyWord16le
  addr <- Parse.take (if addrType == IPV4 then 4 else 16)
  pure $ Address
    { addressType    = addrType
    , addressPort    = addrPort
    , addressAddress = addr
    }

messageParser :: MessageType -> Parser Message
messageParser MPingType = MPing <$> anyWord64le

messageParser MPongType = MPong <$> anyWord64le

messageParser MRejectType = MReject . Reject <$> rejectCodeParser

messageParser MVersionType = do
  version       <- versionParser
  time          <- fromIntegral <$> anyWord64le
  nonce         <- anyWord64le
  currencies    <- anyWord32le
  versionBlocks <- UV.fromList <$> replicateM (fromIntegral currencies) versionBlockParser

  pure $ MVersion $ Version
    { versionVersion    = version
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = versionBlocks
    }

messageParser MVersionACKType = MVersionACK VersionACK <$ word8 0

messageParser MFiltersRequestType = do
  currency <- currencyCodeParser
  start    <- anyWord64le
  amount   <- anyWord64le

  pure $ MFiltersRequest $ FilterRequest
    { filterRequestMsgCurrency = currency
    , filterRequestMsgStart    = start
    , filterRequestMsgAmount   = amount
    }

messageParser MFiltersResponseType = do
  currency <- currencyCodeParser
  amount <- anyWord32le
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
  height <- anyWord64le
  blockIdLength <- fromIntegral <$> anyWord32le
  blockId <- Parse.take blockIdLength
  blockFilterLength <- fromIntegral <$> anyWord32le
  blockFilter <- Parse.take blockFilterLength

  pure $ MFiltersEvent $ FilterEvent
    { filterEventCurrency    = currency
    , filterEventHeight      = height
    , filterEventBlockId     = BSS.toShort blockId
    , filterEventBlockFilter = blockFilter
    }

messageParser MFeeRequestType = do
  amount <- anyWord32le
  curs <- replicateM (fromIntegral amount) currencyCodeParser
  pure $ MFeeRequest curs

messageParser MFeeResponseType = do
  amount <- anyWord32le
  resps <- replicateM (fromIntegral amount) parseFeeResp
  pure $ MFeeResponse resps

messageParser MPeerRequestType = MPeerRequest PeerRequest <$ word8 0

messageParser MPeerResponseType = do
  amount <- anyWord32le
  addresses <- V.fromList <$> replicateM (fromIntegral amount) addressParser
  pure $ MPeerResponse $  PeerResponse
    { peerResponseAddresses = addresses
    }

messageParser MIntroducePeerType = do
  amount <- anyWord32le
  addresses <- V.fromList <$> replicateM (fromIntegral amount) addressParser
  pure $ MPeerIntroduce $ PeerIntroduce
    { peerIntroduceAddresses = addresses
    }

messageParser MRatesRequestType = do
  n <- fmap fromIntegral anyWord32le
  cfs <- replicateM n cfParser
  pure $ MRatesRequest $ RatesRequest $ M.fromList cfs

messageParser MRatesResponseType = do
  n <- fmap fromIntegral anyWord32le
  cfds <- replicateM n cfdParser
  pure $ MRatesResponse $ RatesResponse $ M.fromList cfds

enumParser :: Enum a => Parser a
enumParser = fmap (toEnum . fromIntegral) anyWord32le

cfParser :: Parser (CurrencyCode, [Fiat])
cfParser = do
  c <- enumParser
  n <- fmap fromIntegral anyWord32le
  fmap (c, ) $ replicateM n enumParser

cfdParser :: Parser (CurrencyCode, M.Map Fiat Double)
cfdParser = do
  c <- enumParser
  n <- fmap fromIntegral anyWord32le
  fmap ((c,) . M.fromList) $ replicateM n fdParser

fdParser :: Parser (Fiat, Double)
fdParser = (,) <$> enumParser <*> parseDouble

parseDouble :: Parser Double
parseDouble = do
  c <- fromIntegral <$> anyWord64le
  e <- fromIntegral <$> anyWord64le
  pure $ toRealFloat $ scientific c e

parseCurrencyPair :: Parser (CurrencyCode, Fiat)
parseCurrencyPair = (,)
  <$> (fmap (toEnum . fromIntegral) anyWord32le)
  <*> (fmap (toEnum . fromIntegral) anyWord32le)

parseFeeResp :: Parser FeeResp
parseFeeResp = do
  currency <- currencyCodeParser
  case currency of
    BTC -> btcParser False
    TBTC -> btcParser True
    _ -> genericParser currency
  where
    btcParser isTest = do
      h <- (,) <$> anyWord64le <*> anyWord64le
      m <- (,) <$> anyWord64le <*> anyWord64le
      l <- (,) <$> anyWord64le <*> anyWord64le
      pure $ FeeRespBTC isTest $ FeeBundle h m l
    genericParser cur = FeeRespGeneric cur
      <$> anyWord64le
      <*> anyWord64le
      <*> anyWord64le

parseMessage :: MessageType -> BS.ByteString -> Either String (Message, BS.ByteString)
parseMessage msgType source =
  case parse (messageParser msgType) source of
    Done rest message -> Right (message, rest)
    Partial _ -> Left "source too short"
    Fail _ _ err -> Left err
