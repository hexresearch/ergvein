module Test.Persist
  (
    buildVarIntP
  , buildTxOutP
  , buildTxHashP
  , buildOutPointP
  , buildTxInP
  , buildWitnessDataP
  , buildTxInOutP
  , buildLegacyTxP
  , buildWitnessTxP
  , buildTxP
  , decVarInt

  , buildBS
  , buildBSS
  , buildVarInt
  , buildTxOut
  , buildTxHash
  , buildOutPoint
  , buildTxIn
  , buildWitnessData
  , buildTxInOut
  , buildLegacyTx
  , buildWitnessTx
  , buildTx
  ) where

import qualified Data.Persist as P
import Network.Haskoin.Crypto (getHash256, doubleSHA256)
import Data.Word
import Network.Haskoin.Transaction hiding (buildTx)
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString hiding (word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import Data.ByteString.Builder

decVarInt :: BS.ByteString -> Either String Word64
decVarInt = parseOnly parseVarInt

parseVarInt :: Parser Word64
parseVarInt = anyWord8 >>= go
  where
    go 0xff = anyWord64le
    go 0xfe = fromIntegral <$> anyWord32le
    go 0xfd = fromIntegral <$> anyWord16le
    go x    = fromIntegral <$> return x

buildVarIntP :: Word64 -> P.Put ()
buildVarIntP x
  | x < 0xfd        = P.put @Word8 $ fromIntegral x
  | x <= 0xffff     = P.put @Word8 0xfd >> (P.put @Word16 $ fromIntegral x)
  | x <= 0xffffffff = P.put @Word8 0xfe >> (P.put @Word32 $ fromIntegral x)
  | otherwise       = P.put @Word8 0xff >> (P.put @Word64 x)

buildBSP :: BS.ByteString -> P.Put ()
buildBSP bs = buildVarIntP (fromIntegral $ BS.length bs) >> P.putByteString bs

buildTxOutP :: TxOut -> P.Put ()
buildTxOutP (TxOut o s) = P.put o >> buildBSP s

buildTxHashP :: TxHash -> P.Put ()
buildTxHashP (TxHash h) = P.putByteString $ BSS.fromShort $ getHash256 h

buildOutPointP :: OutPoint -> P.Put ()
buildOutPointP (OutPoint h i) = buildTxHashP h >> P.put i

buildTxInP :: TxIn -> P.Put ()
buildTxInP (TxIn o s q) = buildOutPointP o >> buildBSP s >> P.put q

buildWitnessDataP :: WitnessData -> P.Put ()
buildWitnessDataP wd = flip mapM_ wd $ \ws ->
  buildVarIntP (fromIntegral $ length ws) >> mapM_ buildBSP ws

buildTxInOutP :: Tx -> P.Put ()
buildTxInOutP (Tx _ i o _ _) = do
  buildVarIntP (fromIntegral $ length i)
  mapM_ buildTxInP i
  buildVarIntP (fromIntegral $ length o)
  mapM_ buildTxOutP o

buildLegacyTxP :: Tx -> P.Put ()
buildLegacyTxP tx = do
  P.put (txVersion tx)
  buildTxInOutP tx
  P.put (txLockTime tx)

buildWitnessTxP :: Tx -> P.Put ()
buildWitnessTxP tx = do
  P.put (txVersion tx)
  P.put @Word8 0x00
  P.put @Word8 0x01
  buildTxInOutP tx
  buildWitnessDataP (txWitness tx)
  P.put (txLockTime tx)

buildTxP :: Tx -> P.Put ()
buildTxP tx
  | null (txWitness tx) = buildLegacyTxP tx
  | otherwise = buildWitnessTxP tx


---
buildBS :: BS.ByteString -> Builder
buildBS bs = buildVarInt (fromIntegral $ BS.length bs) <> byteString bs
buildBSS :: BSS.ShortByteString -> Builder
buildBSS = buildBS . BSS.fromShort
buildVarInt :: Word64 -> Builder
buildVarInt x
  | x < 0xfd        = word8 $ fromIntegral x
  | x <= 0xffff     = word8 0xfd <> (word16LE $ fromIntegral x)
  | x <= 0xffffffff = word8 0xfe <> (word32LE $ fromIntegral x)
  | otherwise       = word8 0xff <> (word64LE x)
buildTxOut :: TxOut -> Builder
buildTxOut (TxOut o s) = word64LE o <> buildBS s
buildTxHash :: TxHash -> Builder
buildTxHash (TxHash h) = shortByteString $ getHash256 h
buildOutPoint :: OutPoint -> Builder
buildOutPoint (OutPoint h i) = buildTxHash h <> word32LE i
buildTxIn :: TxIn -> Builder
buildTxIn (TxIn o s q) = buildOutPoint o <> buildBS s <> word32LE q
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
buildTx :: Tx -> Builder
buildTx tx
  | null (txWitness tx) = buildLegacyTx tx
  | otherwise = buildWitnessTx tx
