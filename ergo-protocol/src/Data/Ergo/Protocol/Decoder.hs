module Data.Ergo.Protocol.Decoder(
    decodeMessage
  , decodeHandshake
  , messageParser
  , msgBodyParser
  , handshakeParser
  , parseMsgLength
  , parseMsgBody
  , anyWord32be
  , runGet
  ) where

import Control.Monad.IO.Class
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Bits
import Data.ByteString (ByteString)
import Data.Ergo.Protocol.Check
import Data.Ergo.Protocol.Shift
import Data.Ergo.Protocol.Types
import Data.Int
import Data.IORef
import Data.Persist
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Vector (Vector)
import Data.Word
import Prelude hiding (fail)

import qualified Data.Vector as V
import qualified Data.ByteString as BS

import Debug.Trace

-- | Perform parsing of whole message from bytestring without remainder
decodeMessage :: Network -> ByteString -> Either String Message
decodeMessage = runGet . messageParser

-- | Decode only handshake message
decodeHandshake :: ByteString -> Either String Handshake
decodeHandshake = runGet handshakeParser

embedParser :: String -> Get a -> ByteString -> Get a
embedParser msg p bs = case runGet p bs of
  Left er -> fail $ msg <> ": " <> er
  Right a -> pure a

word32be :: Word32 -> Get Word32
word32be w = do
  v <- fmap unBE get
  unless (v == w) $ fail $ "Expected " <> show w <> " but got " <> show v
  pure v

anyWord8 :: Get Word8
anyWord8 = get

anyInt64be :: Get Int64
anyInt64be = fmap unBE get

anyWord64be :: Get Word64
anyWord64be = fmap unBE get

anyInt32be :: Get Int32
anyInt32be = fmap unBE get

anyWord32be :: Get Word32
anyWord32be = fmap unBE get

anyWord32le :: Get Word32
anyWord32le = fmap unLE get

anyWord16be :: Get Word16
anyWord16be = fmap unBE get

-- | Parse VLQ (variable int) encoding.
--
-- https://ergoplatform.org/docs/ErgoTree.pdf
vlq :: (Integral a, Bits a) => Get a
vlq = go 0 0
  where
    go !i !acc = do
      w <- fromIntegral <$> anyWord8
      if w .&. complement 0x7F == 0
        then pure $ acc + shiftL w i
        else go (i+7) (acc + shiftL (w .&. 0x7F) i)

vlqInt64 :: Get Int64
vlqInt64 = vlq

vlqWord16 :: Get Word16
vlqWord16 = vlq

vlqWord32 :: Get Word32
vlqWord32 = vlq

-- | Decode ZigZag encoded signed integer
--
--  http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java/core/src/main/java/com/google/protobuf/CodedInputStream.java#L553
zigzag :: (Integral a, Bits a, ShiftRS a) => a -> a
zigzag n = (n `shiftRS` 1) `xor` (-(n .&. 1))

varInt32 :: Get Int32
varInt32 = zigzag <$> vlq

-- | Parse first 4+1+4=9 bytes and return length of rest message.
--
-- The helper designed for usage in sockets.
--
-- First int is message id, second is message length in bytes.
parseMsgLength :: Network -> ByteString -> Either String (Int, Int)
parseMsgLength = runGet . lengthParser

lengthParser :: Network -> Get (Int, Int)
lengthParser net = do
  (mb, i, l) <- headerParser
  unless (magicBytes net == mb) $ fail $ "Wrong magic bytes of message! Expected: "
    <> show (magicBytes net) <> ", but got: " <> show mb
  pure (fromIntegral i, fromIntegral l)

-- | Parser of magicBytes, message type and length
headerParser :: Get (Word32, Word8, Word32)
headerParser = (,,) <$> anyWord32be <*> anyWord8 <*> anyWord32be

-- | Parse message with preceding id and length
messageParser :: Network -> Get Message
messageParser net = do
  (i, l) <- lengthParser net
  msgBodyParser i l

parseMsgBody :: Int -> Int -> ByteString -> Either String Message
parseMsgBody i l = runGet $ msgBodyParser i l

-- | Parse message without id and length
msgBodyParser :: Int -> Int -> Get Message
msgBodyParser i l = do
  c <- getByteString 4
  body <- getByteString (fromIntegral l)
  traceShowM (i, body)
  unless (validateSum c body) $ fail "Check sum failed for body!"
  if | i == syncInfoId -> MsgSyncInfo <$> embedParser "SyncInfo parsing error" syncInfoParser body
     | otherwise -> fail $ "Unknown message type " <> show i

parseText :: Get Text
parseText = parseTextN =<< anyWord8

parseTextN :: Word8 -> Get Text
parseTextN l = do
  bs <- getByteString (fromIntegral l)
  pure $ decodeUtf8With lenientDecode bs

parseVersion :: Get ProtoVer
parseVersion = ProtoVer <$> anyWord8 <*> anyWord8 <*> anyWord8

parseOptional :: Get a -> Get (Maybe a)
parseOptional p = do
  f <- anyWord8
  if f == 0 then pure Nothing else Just <$> p

parseVector :: Get a -> Get (Vector a)
parseVector p = do
  l <- anyWord8
  traceShowM l
  V.replicateM (fromIntegral l) p

parseIP :: Get IP
parseIP = parseIPN =<< anyWord8

parseIPN :: Word8 -> Get IP
parseIPN l
   | l == 4 = IPV4 <$> anyWord32le
   | l == 16 = IPV6 <$> anyWord32le <*> anyWord32le <*> anyWord32le <*> anyWord32le
   | otherwise = fail $ "Unknown network address with size " <> show l

parseNetAddr :: Get NetAddr
parseNetAddr = NetAddr <$> parseIP <*> anyWord32be

parseFlag :: Get Bool
parseFlag = do
  v <- anyWord8
  pure $ if v == 0 then False else True

parseStateType :: Get StateType
parseStateType = do
  i <- anyWord8
  if | i == 0 -> pure StateUtxo
     | i == 1 -> pure StateDigest
     | otherwise -> fail $ "Unknown StateType enum value " <> show i

parseOperationMode :: Get OperationModeFeature
parseOperationMode = OperationModeFeature
  <$> parseStateType
  <*> parseFlag
  <*> parseOptional anyWord32be
  <*> varInt32

parseSessionFeature :: Get SessionFeature
parseSessionFeature = SessionFeature
  <$> anyWord32be
  <*> anyWord64be

parsePeerFeature :: Get PeerFeature
parsePeerFeature = do
  i <- anyWord8
  l <- vlqWord16
  traceShowM (i, l)
  body <- getBytes $ fromIntegral l
  embedParser ("Feature " <> show i <> " body parsing error") (parsePeerFeatureN i l) body

parsePeerFeatureN :: Word8 -> Word16 -> Get PeerFeature
parsePeerFeatureN i l
  | i == featureOperationModeId = FeatureOperationMode <$> parseOperationMode
  | i == sessionFeatureId = FeatureSession <$> parseSessionFeature
  | otherwise = UnknownFeature i <$> (fmap BS.copy $ getByteString (fromIntegral l))

handshakeParser :: Get Handshake
handshakeParser = Handshake
  <$> vlqInt64
  <*> parseText
  <*> parseVersion
  <*> parseText
  <*> parseOptional parseNetAddr
  <*> parseVector parsePeerFeature

syncInfoParser :: Get SyncInfo
syncInfoParser = SyncInfo
  <$> parseVector headerIdParser

headerIdParser :: Get HeaderId
headerIdParser = HeaderId <$> getBytes 32
