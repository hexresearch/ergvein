module Data.Ergo.Protocol.Decoder(
    decodeMessage
  , messageParser
  ) where

import Control.Monad
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import Data.Ergo.Protocol.Check
import Data.Ergo.Protocol.Types
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Vector (Vector)

import qualified Data.Vector as V
import qualified Data.ByteString as BS

-- | Perform parsing of whole message from bytestring without remainder
decodeMessage :: Network -> ByteString -> Either String Message
decodeMessage net = parseOnly (messageParser net <* endOfInput)

embedParser :: String -> Parser a -> ByteString -> Parser a
embedParser msg p bs = case parseOnly (p <* endOfInput) bs of
  Left er -> fail $ msg <> ": " <> er
  Right a -> pure a

messageParser :: Network -> Parser Message
messageParser net = do
  _ <- word32be $ magicBytes net
  i <- anyWord8
  l <- anyWord32be
  body <- A.take (fromIntegral l)
  c <- A.take 4
  unless (validateSum c body) $ fail "Check sum failed for body!"
  if | i == handshakeId -> MsgHandshake <$> embedParser "Handshake parsing error" handshakeParser body
     | otherwise -> fail $ "Unknown message type " <> show i

parseText :: Parser Text
parseText = do
  l <- anyWord8
  bs <- A.take (fromIntegral l)
  pure $ decodeUtf8With lenientDecode bs

parseVersion :: Parser ProtoVer
parseVersion = ProtoVer <$> anyWord8 <*> anyWord8 <*> anyWord8

parseOptional :: Parser a -> Parser (Maybe a)
parseOptional p = do
  f <- anyWord8
  if f == 0 then pure Nothing else Just <$> p

parseVector :: Parser a -> Parser (Vector a)
parseVector p = do
  l <- anyWord8
  V.replicateM (fromIntegral l) p

parseIP :: Parser IP
parseIP = do
  l <- anyWord8
  if | l == 4 -> IPV4 <$> anyWord32le
     | l == 16 -> IPV6 <$> anyWord32le <*> anyWord32le <*> anyWord32le <*> anyWord32le
     | otherwise -> fail $ "Unknown network address with size " <> show l

parseNetAddr :: Parser NetAddr
parseNetAddr = NetAddr <$> parseIP <*> anyWord32be

parseFlag :: Parser Bool
parseFlag = do
  v <- anyWord8
  pure $ if v == 0 then False else True

parseStateType :: Parser StateType
parseStateType = do
  i <- anyWord8
  if | i == 0 -> pure StateUtxo
     | i == 1 -> pure StateDigest
     | otherwise -> fail $ "Unknown StateType enum value " <> show i

parseOperationMode :: Parser OperationModeFeature
parseOperationMode = OperationModeFeature
  <$> parseStateType
  <*> parseFlag
  <*> parseOptional anyWord32be
  <*> parseBlocksStored
  where
    parseBlocksStored = do
      w <- anyWord32be
      let i = fromIntegral w :: Int32
      pure $ if i < 0 then Nothing else Just w

parsePeerFeature :: Parser PeerFeature
parsePeerFeature = do
  i <- anyWord8
  l <- anyWord16be
  if | i == featureOperationModeId -> FeatureOperationMode <$> parseOperationMode
     | otherwise -> UnknownFeature i <$> (fmap BS.copy $ A.take (fromIntegral l))


handshakeParser :: Parser Handshake
handshakeParser = Handshake
  <$> fmap fromIntegral anyWord64be
  <*> parseText
  <*> parseVersion
  <*> parseText
  <*> parseOptional parseNetAddr
  <*> parseVector parsePeerFeature
