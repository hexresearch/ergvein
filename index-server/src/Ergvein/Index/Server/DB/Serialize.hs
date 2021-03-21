module Ergvein.Index.Server.DB.Serialize
  (
    serializeVarInt
  , deserializeVarInt
  , encodeOutPoint
  , encodeTxHash
  , encodeBtcTxHash
  , encodeWord32
  ) where

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString hiding (word8)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Serialize as S
import Data.Word
import Network.Haskoin.Transaction (OutPoint(..), getTxHash)
import Network.Haskoin.Crypto (getHash256)
import Ergvein.Types.Transaction

import qualified Network.Haskoin.Transaction as HK
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS

serializeVarInt :: Word64 -> ByteString
serializeVarInt = BL.toStrict . toLazyByteString . buildVarInt

buildVarInt :: Word64 -> Builder
buildVarInt x
  | x < 0xfd        = word8 $ fromIntegral x
  | x <= 0xffff     = word8 0xfd <> (word16LE $ fromIntegral x)
  | x <= 0xffffffff = word8 0xfe <> (word32LE $ fromIntegral x)
  | otherwise       = word8 0xff <> (word64LE x)

deserializeVarInt :: ByteString -> Either String Word64
deserializeVarInt = parseOnly parseVarInt

parseVarInt :: Parser Word64
parseVarInt = anyWord8 >>= go
  where
    go 0xff = anyWord64le
    go 0xfe = fromIntegral <$> anyWord32le
    go 0xfd = fromIntegral <$> anyWord16le
    go x    = fromIntegral <$> return x

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
encodeWord32 = BL.toStrict . toLazyByteString . word32LE
