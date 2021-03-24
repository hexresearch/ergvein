module Ergvein.Index.Server.DB.Serialize
  (
    serializeVarInt
  , deserializeVarInt
  , encodeOutPoint
  , encodeTxHash
  , encodeBtcTxHash
  , encodeWord32
  , encodeWord64
  , decodeWord32
  , decodeWord64
  , encodeHkTx
  , calcHkTxHash
  , hkTxHash
  , serializeCache
  , deserializeCache
  ) where

import Control.DeepSeq
import Control.Monad
import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteString (ByteString)
import Data.Serialize as S
import Data.Word
import Network.Haskoin.Crypto (getHash256, doubleSHA256)
import Network.Haskoin.Transaction (OutPoint(..), getTxHash)
import Network.Haskoin.Transaction hiding (TxHash)

import Ergvein.Types.Transaction
import Ergvein.Index.Server.Types

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Persist as P
import qualified Data.Sequence as Seq
import qualified Network.Haskoin.Transaction as HK

serializeVarInt :: Word64 -> ByteString
serializeVarInt = P.runPut . putVarInt

deserializeVarInt :: ByteString -> Either String Word64
deserializeVarInt = P.runGet parseVarInt

encodeOutPoint :: OutPoint -> ByteString
encodeOutPoint (OutPoint th i) = encodeBtcTxHash th <> encodeWord32 i
{-# INLINE encodeOutPoint #-}

-- | Encode txhash, ignoring the currency
encodeTxHash :: TxHash -> ByteString
encodeTxHash th = case th of
  BtcTxHash bth -> BSS.fromShort $ getHash256 $ getTxHash bth
  ErgTxHash eth -> S.encode eth
{-# INLINE encodeTxHash #-}

encodeBtcTxHash :: HK.TxHash -> ByteString
encodeBtcTxHash = BSS.fromShort . getHash256 . getTxHash
{-# INLINE encodeBtcTxHash #-}

encodeWord32 :: Word32 -> ByteString
encodeWord32 = P.runPut . P.put
{-# INLINE encodeWord32 #-}

encodeWord64 :: Word64 -> ByteString
encodeWord64 = P.runPut . P.put
{-# INLINE encodeWord64 #-}

decodeWord32 :: ByteString -> Either String Word32
decodeWord32 = P.runGet P.get
{-# INLINE decodeWord32 #-}

decodeWord64 :: ByteString -> Either String Word64
decodeWord64 = P.runGet P.get
{-# INLINE decodeWord64 #-}

encodeHkTx :: Tx -> ByteString
encodeHkTx = P.runPut . putTx
{-# INLINE encodeHkTx #-}

calcHkTxHash :: Tx -> HK.TxHash
calcHkTxHash tx = HK.TxHash $ doubleSHA256 $ encodeHkTx $ tx {txWitness = []}
{-# INLINE calcHkTxHash #-}

hkTxHash :: Tx -> ByteString
hkTxHash tx = force $ BA.convert . hashWith SHA256 . hashWith SHA256 . encodeHkTx $ tx {txWitness = []}
{-# INLINE hkTxHash #-}

-- Cache

serializeCache :: Seq.Seq CacheEntry -> ByteString
serializeCache s = P.runPut $ do
  P.put $ Seq.length s
  mapM_ putCacheEntry s

deserializeCache :: ByteString -> Either String (Seq.Seq CacheEntry)
deserializeCache = P.runGet $ do
  n <- P.get
  s <- replicateM n getCacheEntry
  pure $ Seq.fromList s

putCacheEntry :: CacheEntry -> P.Put ()
putCacheEntry (CacheEntry he ha spent created) = do
  P.put he
  P.put ha
  P.put $ length spent
  mapM_ putOutPoint spent
  P.put $ length created
  P.put created

getCacheEntry :: P.Get CacheEntry
getCacheEntry = do
  he <- P.get
  ha <- P.get
  n <- P.get
  spent <- replicateM n getOutPoint
  m <- P.get
  created <- replicateM m P.get
  pure $ CacheEntry he ha spent created

-- Builders

putVarInt :: Word64 -> P.Put ()
putVarInt x
  | x < 0xfd        = P.put @Word8 $ fromIntegral x
  | x <= 0xffff     = P.put @Word8 0xfd >> (P.put @Word16 $ fromIntegral x)
  | x <= 0xffffffff = P.put @Word8 0xfe >> (P.put @Word32 $ fromIntegral x)
  | otherwise       = P.put @Word8 0xff >> (P.put @Word64 x)

parseVarInt :: P.Get Word64
parseVarInt = P.get @Word8 >>= go
  where
    go 0xff = P.get
    go 0xfe = fromIntegral <$> P.get @Word32
    go 0xfd = fromIntegral <$> P.get @Word16
    go x = pure $ fromIntegral x

putBS :: BS.ByteString -> P.Put ()
putBS bs = putVarInt (fromIntegral $ BS.length bs) >> P.putByteString bs

putTxOut :: TxOut -> P.Put ()
putTxOut (TxOut o s) = P.put o >> putBS s

putTxHash :: HK.TxHash -> P.Put ()
putTxHash (HK.TxHash h) = P.putByteString $ BSS.fromShort $ getHash256 h

getPersistedTxHash :: P.Get HK.TxHash
getPersistedTxHash = do
  bs <- P.getByteString 32
  either fail (pure . HK.TxHash) $ S.decode bs

putOutPoint :: OutPoint -> P.Put ()
putOutPoint (OutPoint h i) = putTxHash h >> P.put i

getOutPoint :: P.Get OutPoint
getOutPoint = OutPoint <$> getPersistedTxHash <*> P.get

putTxIn :: TxIn -> P.Put ()
putTxIn (TxIn o s q) = putOutPoint o >> putBS s >> P.put q

putWitnessData :: WitnessData -> P.Put ()
putWitnessData wd = flip mapM_ wd $ \ws ->
  putVarInt (fromIntegral $ length ws) >> mapM_ putBS ws

putTxInOut :: Tx -> P.Put ()
putTxInOut (Tx _ i o _ _) = do
  putVarInt (fromIntegral $ length i)
  mapM_ putTxIn i
  putVarInt (fromIntegral $ length o)
  mapM_ putTxOut o

putLegacyTx :: Tx -> P.Put ()
putLegacyTx tx = do
  P.put (txVersion tx)
  putTxInOut tx
  P.put (txLockTime tx)

putWitnessTx :: Tx -> P.Put ()
putWitnessTx tx = do
  P.put (txVersion tx)
  P.put @Word8 0x00
  P.put @Word8 0x01
  putTxInOut tx
  putWitnessData (txWitness tx)
  P.put (txLockTime tx)

putTx :: Tx -> P.Put ()
putTx tx
  | null (txWitness tx) = putLegacyTx tx
  | otherwise = putWitnessTx tx
