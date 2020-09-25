-- | Helpers to parse and encode variable length intergers in BTC (aka CompactSize).
-- See https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer
module Ergvein.Filters.Btc.VarInt(
    encodeVarInt
  , parseVarInt
  ) where

import Data.Word
import Network.Haskoin.Network        (VarInt(..))

import qualified Data.Attoparsec.Binary        as A
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.ByteString.Builder       as B
import qualified Data.Serialize                as C

-- | Encode integer as variable length Bitcoin encoding (see https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer)
encodeVarInt :: Word64 -> B.Builder
encodeVarInt = snd . C.runPutMBuilder . C.put . VarInt
{-# INLINE encodeVarInt #-}

-- | Decode integer from variable length Bitcoin encoding (see https://en.bitcoin.it/wiki/Protocol_documentation#Variable_length_integer)
parseVarInt :: A.Parser Word64
parseVarInt = A.anyWord8 >>= go
  where
    go 0xff = A.anyWord64le
    go 0xfe = fromIntegral <$> A.anyWord32le
    go 0xfd = fromIntegral <$> A.anyWord16le
    go x    = pure $ fromIntegral x
{-# INLINE parseVarInt #-}
