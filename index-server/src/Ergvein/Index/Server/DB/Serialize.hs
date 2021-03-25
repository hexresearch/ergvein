module Ergvein.Index.Server.DB.Serialize
  (
    serializeVarInt
  , deserializeVarInt
  , encodeOutPoint
  , encodeBtcTxHash
  , encodeWord32
  , encodeWord64
  , decodeWord32
  , decodeWord64
  , calcHkTxHash
  , hkTxHash
  , serializeCache
  , deserializeCache
  ) where

import Control.DeepSeq
import Control.Monad
import Crypto.Hash (SHA256 (..), hashWith)
import Data.ByteString (ByteString)
import Data.Word
import Network.Haskoin.Crypto (getHash256, doubleSHA256)
import Network.Haskoin.Transaction (OutPoint(..), getTxHash)
import Network.Haskoin.Transaction hiding (TxHash)
import Data.Persist

import Ergvein.Index.Server.Types

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize as S
import qualified Data.Sequence as Seq
import qualified Network.Haskoin.Transaction as HK

serializeVarInt :: Word64 -> ByteString
serializeVarInt = runPut . putVarInt
{-# INLINE serializeVarInt #-}

deserializeVarInt :: ByteString -> Either String Word64
deserializeVarInt = runGet parseVarInt
{-# INLINE deserializeVarInt #-}

encodeOutPoint :: OutPoint -> ByteString
encodeOutPoint (OutPoint th i) = encodeBtcTxHash th <> encodeWord32 i
{-# INLINE encodeOutPoint #-}

encodeBtcTxHash :: HK.TxHash -> ByteString
encodeBtcTxHash = BSS.fromShort . getHash256 . getTxHash
{-# INLINE encodeBtcTxHash #-}

encodeWord32 :: Word32 -> ByteString
encodeWord32 = runPut . put
{-# INLINE encodeWord32 #-}

encodeWord64 :: Word64 -> ByteString
encodeWord64 = runPut . put
{-# INLINE encodeWord64 #-}

decodeWord32 :: ByteString -> Either String Word32
decodeWord32 = runGet get
{-# INLINE decodeWord32 #-}

decodeWord64 :: ByteString -> Either String Word64
decodeWord64 = runGet get
{-# INLINE decodeWord64 #-}

encodeHkTx :: Tx -> ByteString
encodeHkTx = runPut . putTx
{-# INLINE encodeHkTx #-}

calcHkTxHash :: Tx -> HK.TxHash
calcHkTxHash tx = HK.TxHash $ doubleSHA256 $ encodeHkTx $ tx {txWitness = []}
{-# INLINE calcHkTxHash #-}

hkTxHash :: Tx -> ByteString
hkTxHash tx = force $ BA.convert . hashWith SHA256 . hashWith SHA256 . encodeHkTx $ tx {txWitness = []}
{-# INLINE hkTxHash #-}

-- Cache

serializeCache :: Seq.Seq CacheEntry -> ByteString
serializeCache s = runPut $ do
  put $ Seq.length s
  mapM_ putCacheEntry s

deserializeCache :: ByteString -> Either String (Seq.Seq CacheEntry)
deserializeCache = runGet $ do
  n <- get
  s <- replicateM n getCacheEntry
  pure $ Seq.fromList s

putCacheEntry :: CacheEntry -> Put ()
putCacheEntry (CacheEntry he ha spent created) = do
  put he
  put ha
  put $ length spent
  mapM_ putOutPoint spent
  put $ length created
  put created

getCacheEntry :: Get CacheEntry
getCacheEntry = do
  he <- get
  ha <- get
  n <- get
  spent <- replicateM n getOutPoint
  m <- get
  created <- replicateM m get
  pure $ CacheEntry he ha spent created

-- Tx-related Builders

putVarInt :: Word64 -> Put ()
putVarInt x
  | x < 0xfd        = put @Word8 $ fromIntegral x
  | x <= 0xffff     = put @Word8 0xfd >> (put @Word16 $ fromIntegral x)
  | x <= 0xffffffff = put @Word8 0xfe >> (put @Word32 $ fromIntegral x)
  | otherwise       = put @Word8 0xff >> (put @Word64 x)

parseVarInt :: Get Word64
parseVarInt = get @Word8 >>= go
  where
    go 0xff = get
    go 0xfe = fromIntegral <$> get @Word32
    go 0xfd = fromIntegral <$> get @Word16
    go x = pure $ fromIntegral x

putBS :: BS.ByteString -> Put ()
putBS bs = putVarInt (fromIntegral $ BS.length bs) >> putByteString bs

putTxOut :: TxOut -> Put ()
putTxOut (TxOut o s) = put o >> putBS s

putTxHash :: HK.TxHash -> Put ()
putTxHash (HK.TxHash h) = putByteString $ BSS.fromShort $ getHash256 h

getPersistedTxHash :: Get HK.TxHash
getPersistedTxHash = do
  bs <- getByteString 32
  either fail (pure . HK.TxHash) $ S.decode bs

putOutPoint :: OutPoint -> Put ()
putOutPoint (OutPoint h i) = putTxHash h >> put i

getOutPoint :: Get OutPoint
getOutPoint = OutPoint <$> getPersistedTxHash <*> get

putTxIn :: TxIn -> Put ()
putTxIn (TxIn o s q) = putOutPoint o >> putBS s >> put q

putWitnessData :: WitnessData -> Put ()
putWitnessData wd = flip mapM_ wd $ \ws ->
  putVarInt (fromIntegral $ length ws) >> mapM_ putBS ws

putTxInOut :: Tx -> Put ()
putTxInOut (Tx _ i o _ _) = do
  putVarInt (fromIntegral $ length i)
  mapM_ putTxIn i
  putVarInt (fromIntegral $ length o)
  mapM_ putTxOut o

putLegacyTx :: Tx -> Put ()
putLegacyTx tx = do
  put (txVersion tx)
  putTxInOut tx
  put (txLockTime tx)

putWitnessTx :: Tx -> Put ()
putWitnessTx tx = do
  put (txVersion tx)
  put @Word8 0x00
  put @Word8 0x01
  putTxInOut tx
  putWitnessData (txWitness tx)
  put (txLockTime tx)

putTx :: Tx -> Put ()
putTx tx
  | null (txWitness tx) = putLegacyTx tx
  | otherwise = putWitnessTx tx
