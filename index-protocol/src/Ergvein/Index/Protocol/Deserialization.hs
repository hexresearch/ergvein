{-# LANGUAGE MultiWayIf #-}
module Ergvein.Index.Protocol.Deserialization where

import Codec.Compression.GZip
import Control.Monad
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.Fixed
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Typeable
import Data.Word

import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Utils
import Ergvein.Types.Fees
import Ergvein.Types.Currency (Fiat)

import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.Bitstream as S
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
  w <- varInt
  case word32ToCurrencyCode w of
    Nothing -> fail "Invalid currency code"
    Just c  -> pure c

word32toRejectType :: Word32 -> Maybe RejectCode
word32toRejectType = \case
  0  -> Just MessageHeaderParsing
  1  -> Just MessageParsing
  2  -> Just InternalServerError
  3  -> Just ZeroBytesReceived
  4  -> Just VersionNotSupported
  _  -> Nothing

word8toFeeLevel :: Word8 -> Maybe FeeLevel
word8toFeeLevel = \case
  0 -> Just FeeFast
  1 -> Just FeeModerate
  2 -> Just FeeCheap
  _ -> Nothing

varInt :: Integral a => Parser a
varInt = do
  w <- anyWord8
  if | w == 0xFF -> fmap fromIntegral anyWord64le
     | w == 0xFE -> fmap fromIntegral anyWord32le
     | w == 0xFD -> fmap fromIntegral anyWord16le
     | otherwise -> pure $ fromIntegral w

versionParser :: Parser ProtocolVersion
versionParser = do
  bs :: S.Bitstream S.Right <- S.fromBits <$> anyWord32be
  let p    = S.toBits $ S.append pad $ S.take i10 bs
  let mn   = S.toBits $ S.append pad $ S.take i10 $ S.drop i10 bs
  let mj   = S.toBits $ S.append pad $ S.take i10 $ S.drop i20 bs
  pure (mj,mn,p)
  where
    i6,i10,i20 :: Int
    i6 = 6 ; i10 = 10 ; i20 = 20
    pad = S.replicate i6 False

messageHeaderParser ::  Parser MessageHeader
messageHeaderParser = do
    mt <- messageTypeParser
    if not $ messageHasPayload mt then  pure $ MessageHeader mt 0 else do
      messageSize <- varInt
      pure $ MessageHeader mt messageSize

messageLengthParser :: Parser Word32
messageLengthParser = varInt

messageTypeParser :: Parser MessageType
messageTypeParser = guardJust "out of message type bounds" . word32toMessageType =<< varInt

rejectCodeParser :: Parser RejectCode
rejectCodeParser = guardJust "out of reject type bounds" . word32toRejectType =<< varInt

feeLevelParser :: Parser FeeLevel
feeLevelParser = guardJust "out of feeLevel type bounds" . word8toFeeLevel =<< anyWord8

versionBlockParser ::  Parser ScanBlock
versionBlockParser = do
  currency   <- currencyCodeParser
  version    <- versionParser
  scanHeight <- varInt
  height     <- varInt

  pure $ ScanBlock
    { scanBlockCurrency   = currency
    , scanBlockVersion    = version
    , scanBlockScanHeight = scanHeight
    , scanBlockHeight     = height
    }

blockIdLength :: CurrencyCode -> Int
blockIdLength = \case
  BTC -> 32
  TBTC -> 32
  _ -> 32 -- TODO: edit for other currencies if differ

filterParser :: CurrencyCode -> Parser BlockFilter
filterParser c = do
  blockId <- Parse.take $ blockIdLength c
  blockFilterLength <- fromIntegral <$> (varInt :: Parser Word32)
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
  case addrType of
    IPV4 -> AddressIpv4 <$> anyWord32be <*> anyWord16be
    IPV6 -> AddressIpv6 <$> (IpV6 <$> anyWord32be <*> anyWord32be <*> anyWord32be <*> anyWord32be) <*> anyWord16be
    OnionV3 -> AddressOnionV3 <$> Parse.take (fromIntegral $ addressSize OnionV3) <*> anyWord16be

textParser :: Parser Text
textParser = do
  l :: Word32 <- varInt
  bs <- Parse.take (fromIntegral l)
  pure $ decodeUtf8With lenientDecode bs

messageParser :: MessageType -> Parser Message
messageParser MPingType = MPing <$> anyWord64le

messageParser MPongType = MPong <$> anyWord64le

messageParser MRejectType = fmap MReject $ Reject
  <$> messageTypeParser
  <*> rejectCodeParser
  <*> textParser

messageParser MVersionType = do
  version       <- versionParser
  time          <- fromIntegral <$> anyWord64le
  nonce         <- anyWord64le
  currencies    <- varInt :: Parser Word32
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
  start    <- varInt
  amount   <- varInt

  pure $ MFiltersRequest $ FilterRequest
    { filterRequestMsgCurrency = currency
    , filterRequestMsgStart    = start
    , filterRequestMsgAmount   = amount
    }

messageParser MFiltersResponseType = do
  currency <- currencyCodeParser
  amount :: Word32 <- varInt
  filtersString <- takeLazyByteString

  let unzippedFilters = LBS.toStrict $ decompress filtersString
      parser = V.fromList <$> replicateM (fromIntegral amount) (filterParser currency)

  case parseOnly parser unzippedFilters of
    Right parsedFilters -> pure $ MFiltersResponse $ FilterResponse
      { filterResponseCurrency = currency
      , filterResponseFilters  = parsedFilters
      }
    _ -> fail "fail to parse response filters"


messageParser MFilterEventType = do
  currency <- currencyCodeParser
  height <- varInt
  blockId <- Parse.take (blockIdLength currency)
  blockFilterLength <- fromIntegral <$> (varInt :: Parser Word32)
  blockFilter <- Parse.take blockFilterLength

  pure $ MFiltersEvent $ FilterEvent
    { filterEventCurrency    = currency
    , filterEventHeight      = height
    , filterEventBlockId     = BSS.toShort blockId
    , filterEventBlockFilter = blockFilter
    }

messageParser MFeeRequestType = do
  amount :: Word32 <- varInt
  curs <- replicateM (fromIntegral amount) currencyCodeParser
  pure $ MFeeRequest curs

messageParser MFeeResponseType = do
  amount :: Word32 <- varInt
  resps <- replicateM (fromIntegral amount) parseFeeResp
  pure $ MFeeResponse resps

messageParser MPeerRequestType = pure $ MPeerRequest PeerRequest

messageParser MPeerResponseType = do
  amount :: Word32 <- varInt
  addresses <- V.fromList <$> replicateM (fromIntegral amount) addressParser
  pure $ MPeerResponse $  PeerResponse
    { peerResponseAddresses = addresses
    }

messageParser MIntroducePeerType = do
  amount :: Word32 <- varInt
  addresses <- V.fromList <$> replicateM (fromIntegral amount) addressParser
  pure $ MPeerIntroduce $ PeerIntroduce
    { peerIntroduceAddresses = addresses
    }

messageParser MRatesRequestType = do
  n <- fmap fromIntegral (varInt :: Parser Word32)
  cfs <- replicateM n cfParser
  pure $ MRatesRequest $ RatesRequest $ M.fromList cfs

messageParser MRatesResponseType = do
  n <- fmap fromIntegral (varInt :: Parser Word32)
  cfds <- replicateM n cfdParser
  pure $ MRatesResponse $ RatesResponse $ M.fromList cfds

enumParser :: forall a. (Typeable a, Bounded a, Enum a) => Parser a
enumParser = do
  n <- fromIntegral <$> varInt @Word32
  when (n < lo || n > hi)
    $ fail $ "Enumeration "++show (typeRep (Proxy @a))++" is out of bounds ["
       ++ show lo ++ "," ++ show hi ++ "]: " ++ show n
  pure $! toEnum n
  where
    lo = fromEnum (minBound @a)
    hi = fromEnum (maxBound @a)

cfParser :: Parser (CurrencyCode, [Fiat])
cfParser = do
  c <- enumParser
  n <- fmap fromIntegral (varInt :: Parser Word32)
  fmap (c, ) $ replicateM n enumParser

cfdParser :: Parser (CurrencyCode, M.Map Fiat Centi)
cfdParser = do
  c <- enumParser
  n <- fmap fromIntegral (varInt :: Parser Word32)
  fmap ((c,) . M.fromList) $ replicateM n fdParser

fdParser :: Parser (Fiat, Centi)
fdParser = (,) <$> enumParser <*> parseCenti

parseCenti :: Parser Centi
parseCenti = do
  w <- anyWord64le
  pure $ MkFixed $ fromIntegral w

parseCurrencyPair :: Parser (CurrencyCode, Fiat)
parseCurrencyPair = (,) <$> enumParser <*> enumParser

parseFeeResp :: Parser FeeResp
parseFeeResp = do
  currency <- currencyCodeParser
  case currency of
    BTC -> btcParser False
    TBTC -> btcParser True
    _ -> genericParser currency
  where
    btcParser isTest = do
      h <- (,) <$> varInt <*> varInt
      m <- (,) <$> varInt <*> varInt
      l <- (,) <$> varInt <*> varInt
      pure $ FeeRespBTC isTest $ FeeBundle h m l
    genericParser cur = FeeRespGeneric cur
      <$> varInt
      <*> varInt
      <*> varInt
