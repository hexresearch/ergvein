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

import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString hiding (word8)
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.Word

import Ergvein.Types.Currency

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as BL

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
  egvDeserialize _ = parseOnly anyWord32le

instance EgvSerialize ByteString where
  egvSerialize _ bs = BL.toStrict . toLazyByteString $
    word16LE (fromIntegral $ BS.length bs) <> byteString bs
  egvDeserialize _ = parseOnly $ A.take . fromIntegral =<< anyWord16le
