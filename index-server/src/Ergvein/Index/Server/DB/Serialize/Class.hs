module Ergvein.Index.Server.DB.Serialize.Class
  (
    EgvSerialize(..)
  , getTxHashLength
  , getBlockHashLength
  -- * Common parsers
  , parseVarInt
  , buildVarInt
  , buildBS
  , parseBS
  , buildBSS
  , parseBSS
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString hiding (word8)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Either
import Data.Word
import Network.Haskoin.Crypto
import Network.Haskoin.Transaction hiding (buildTx)

import Ergvein.Index.Protocol.Utils
import Ergvein.Types.Currency

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Serialize as S

-- ===========================================================================
--           Custom serialize-deserialize class
-- ===========================================================================

class EgvSerialize a where
  egvSerialize :: Currency -> a -> ByteString
  egvDeserialize :: Currency -> ByteString -> Either String a

getTxHashLength :: Currency -> Int
getTxHashLength cur = case cur of
  ERGO -> 0 --TODO: Add Ergo lengths
  BTC -> 32
{-# INLINE getTxHashLength #-}

getBlockHashLength :: Currency -> Int
getBlockHashLength cur = case cur of
  ERGO -> 0 --TODO: Add Ergo lengths
  BTC -> 32
{-# INLINE getBlockHashLength #-}

parseVarInt :: Parser Word64
parseVarInt = anyWord8 >>= go
  where
    go 0xff = anyWord64le
    go 0xfe = fromIntegral <$> anyWord32le
    go 0xfd = fromIntegral <$> anyWord16le
    go x    = fromIntegral <$> return x

buildVarInt :: Word64 -> Builder
buildVarInt x
  | x < 0xfd        = word8 $ fromIntegral x
  | x <= 0xffff     = word8 0xfd <> (word16LE $ fromIntegral x)
  | x <= 0xffffffff = word8 0xfe <> (word32LE $ fromIntegral x)
  | otherwise       = word8 0xff <> (word64LE x)

buildBS :: BS.ByteString -> Builder
buildBS bs = buildVarInt (fromIntegral $ BS.length bs) <> byteString bs

parseBS :: Parser BS.ByteString
parseBS = A.take . fromIntegral =<< parseVarInt

buildBSS :: BSS.ShortByteString -> Builder
buildBSS = buildBS . BSS.fromShort

parseBSS :: Parser BSS.ShortByteString
parseBSS = BSS.toShort <$> parseBS


-- ===========================================================================
--           Instances
-- ===========================================================================

instance EgvSerialize Word32 where
  egvSerialize _ = BL.toStrict . toLazyByteString . word32LE
  egvDeserialize _ = parseTillEndOfInput anyWord32le

instance EgvSerialize ByteString where
  egvSerialize _ bs = BL.toStrict . toLazyByteString $
    word16LE (fromIntegral $ BS.length bs) <> byteString bs
  egvDeserialize _ = parseTillEndOfInput $ A.take . fromIntegral =<< anyWord16le


-- ===========================================================================
--           Haskoin data types
-- ===========================================================================

instance EgvSerialize Tx where
  egvSerialize _ = BL.toStrict . toLazyByteString . buildTx
  egvDeserialize _ = parseTillEndOfInput parseTx

instance EgvSerialize TxIn where
  egvSerialize _ = BL.toStrict . toLazyByteString . buildTxIn
  egvDeserialize _ = parseTillEndOfInput parseTxIn

instance EgvSerialize TxOut where
  egvSerialize _ = BL.toStrict . toLazyByteString . buildTxOut
  egvDeserialize _ = parseTillEndOfInput parseTxOut

buildTxOut :: TxOut -> Builder
buildTxOut (TxOut o s) = word64LE o <> buildBS s

parseTxOut :: Parser TxOut
parseTxOut = TxOut <$> anyWord64le <*> parseBS

parseTxHash :: Parser TxHash
parseTxHash = do
  bs <- A.take 32
  pure $ fromRight (error $ "Error decoding TxHash: " <> show bs) $ S.decode bs

buildTxHash :: TxHash -> Builder
buildTxHash (TxHash h) = shortByteString $ getHash256 h

parseOutPoint :: Parser OutPoint
parseOutPoint = OutPoint <$> parseTxHash <*> anyWord32le

buildOutPoint :: OutPoint -> Builder
buildOutPoint (OutPoint h i) = buildTxHash h <> word32LE i

buildTxIn :: TxIn -> Builder
buildTxIn (TxIn o s q) = buildOutPoint o <> buildBS s <> word32LE q

parseTxIn :: Parser TxIn
parseTxIn = TxIn <$> parseOutPoint <*> parseBS <*> anyWord32le

-- | Witness data deserializer. Requires count of inputs.
parseWitnessData :: Int -> Parser WitnessData
parseWitnessData n = replicateM n $ do
  i <- parseVarInt
  replicateM (fromIntegral i) $ parseBS

buildWitnessData :: WitnessData -> Builder
buildWitnessData wd = flip foldMap wd $ \ws ->
  buildVarInt (fromIntegral $ length ws) <> foldMap buildBS ws

buildTxInOut :: Tx -> Builder
buildTxInOut (Tx _ i o _ _) =
       buildVarInt (fromIntegral $ length i)
    <> foldMap buildTxIn i
    <> buildVarInt (fromIntegral $ length o)
    <> foldMap buildTxOut o

buildLegacyTx :: Tx -> Builder
buildLegacyTx tx =
     word32LE (txVersion tx)
  <> buildTxInOut tx
  <> word32LE (txLockTime tx)

buildWitnessTx :: Tx -> Builder
buildWitnessTx tx =
     word32LE (txVersion tx)
  <> word8 0x00
  <> word8 0x01
  <> buildTxInOut tx
  <> buildWitnessData (txWitness tx)
  <> word32LE (txLockTime tx)

parseLegacyTx :: Parser Tx
parseLegacyTx = do
  v <- anyWord32le
  i <- replicateList parseTxIn =<< parseVarInt
  o <- replicateList parseTxOut =<< parseVarInt
  l <- anyWord32le
  pure $ Tx v i o [] l
  where
    replicateList f n = replicateM (fromIntegral n) f

parseWitnessTx :: Parser Tx
parseWitnessTx = do
  v <- anyWord32le
  void $ A.word8 0x00
  void $ A.word8 0x01
  i <- replicateList parseTxIn =<< parseVarInt
  o <- replicateList parseTxOut =<< parseVarInt
  w <- parseWitnessData $ length i
  l <- anyWord32le
  pure $ Tx v i o w l
  where
    replicateList f n = replicateM (fromIntegral n) f

buildTx :: Tx -> Builder
buildTx tx
  | null (txWitness tx) = buildLegacyTx tx
  | otherwise = buildWitnessTx tx

parseTx :: Parser Tx
parseTx = parseWitnessTx <|> (parseLegacyTx <* endOfInput)
