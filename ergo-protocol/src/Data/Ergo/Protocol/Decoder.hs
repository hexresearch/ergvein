module Data.Ergo.Protocol.Decoder(
    decodeMessage
  , messageParser
  , parseMsgLength
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Ergo.Protocol.Check
import Data.Ergo.Protocol.Types
import Data.Int
import Data.Persist
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Vector (Vector)
import Data.Word

import qualified Data.Vector as V
import qualified Data.ByteString as BS

-- | Perform parsing of whole message from bytestring without remainder
decodeMessage :: Network -> ByteString -> Either String Message
decodeMessage net = runGet $ messageParser net

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

-- | Parse first 4+1+4=9 bytes and return length of rest message.
--
-- The helper designed for usage in sockets.
parseMsgLength :: Network -> ByteString -> Either String Int
parseMsgLength net bs = do
  (mb, _, l) <- runGet headerParser bs
  unless (magicBytes net == mb) $ fail "Wrong magic bytes of message!"
  pure $ fromIntegral l

-- | Parser of magicBytes, message type and length
headerParser :: Get (Word32, Word8, Word32)
headerParser = (,,) <$> anyWord32be <*> anyWord8 <*> anyWord32be

messageParser :: Network -> Get Message
messageParser net = do
  (mb, i, l) <- headerParser
  unless (magicBytes net == mb) $ fail "Wrong magic bytes of message!"
  body <- getByteString (fromIntegral l)
  c <- getByteString 4
  unless (validateSum c body) $ fail "Check sum failed for body!"
  if | i == handshakeId -> MsgHandshake <$> embedParser "Handshake parsing error" handshakeParser body
     | otherwise -> fail $ "Unknown message type " <> show i

parseText :: Get Text
parseText = do
  l <- anyWord8
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
  V.replicateM (fromIntegral l) p

parseIP :: Get IP
parseIP = do
  l <- anyWord8
  if | l == 4 -> IPV4 <$> anyWord32le
     | l == 16 -> IPV6 <$> anyWord32le <*> anyWord32le <*> anyWord32le <*> anyWord32le
     | otherwise -> fail $ "Unknown network address with size " <> show l

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
  <*> anyInt32be

parsePeerFeature :: Get PeerFeature
parsePeerFeature = do
  i <- anyWord8
  l <- anyWord16be
  if | i == featureOperationModeId -> FeatureOperationMode <$> parseOperationMode
     | otherwise -> UnknownFeature i <$> (fmap BS.copy $ getByteString (fromIntegral l))


handshakeParser :: Get Handshake
handshakeParser = Handshake
  <$> fmap fromIntegral anyWord64be
  <*> parseText
  <*> parseVersion
  <*> parseText
  <*> parseOptional parseNetAddr
  <*> parseVector parsePeerFeature
