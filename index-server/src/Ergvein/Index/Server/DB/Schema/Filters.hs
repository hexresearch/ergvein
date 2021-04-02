{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Filters
  (
    ScannedHeightRecKey(..)
  , ScannedHeightRec(..)
  , BlockInfoRecKey(..)
  , BlockInfoRec(..)
  , scannedHeightKey
  , blockInfoRecKey
  , unPrefixedKey
  , schemaVersionRecKey
  , schemaVersion
  ) where

import Crypto.Hash.SHA256
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString as Parse
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.FileEmbed
import GHC.Generics
import Data.Serialize (Serialize)

import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Protocol.Utils

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Short   as BSS
import qualified Data.Serialize          as S
import qualified Data.ByteString.Builder as BB

data KeyPrefix
  = SchemaVersion
  | ScannedHeight
  | BlockInfoTag
  deriving Enum

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

-- ===========================================================================
--           Schema Version
-- ===========================================================================

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Filters.hs")

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

-- ===========================================================================
--           Block Info record
-- ===========================================================================

blockInfoRecKey :: (Currency, BlockHeight) -> ByteString
blockInfoRecKey = keyString BlockInfoTag . uncurry BlockInfoRecKey

data BlockInfoRecKey = BlockInfoRecKey
  { blockInfoRecKeyCurrency     :: !Currency
  , blockInfoRecKeyBlockHeight  :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord, Serialize)

data BlockInfoRec = BlockInfoRec
  { blockInfoRecHeaderHash     :: !ShortByteString
  , blockInfoRecAddressFilter  :: !ByteString
  } deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Scanned height record
-- ===========================================================================

scannedHeightKey :: Currency -> ByteString
scannedHeightKey = keyString ScannedHeight . ScannedHeightRecKey

data ScannedHeightRecKey = ScannedHeightRecKey
  { scannedHeightRecKey      :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data ScannedHeightRec = ScannedHeightRec
  { scannedHeightRecHeight   :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           instances EgvSerialize
-- ===========================================================================

instance EgvSerialize ScannedHeightRec where
  egvSerialize _ (ScannedHeightRec sh) = BL.toStrict . BB.toLazyByteString $ BB.word64LE sh
  egvDeserialize _ = parseTillEndOfInput $ ScannedHeightRec <$> anyWord64le

instance EgvSerialize BlockInfoRec where
  egvSerialize _ (BlockInfoRec hd filt) = BL.toStrict . BB.toLazyByteString $ let
    len = fromIntegral $ BS.length filt
    in BB.shortByteString hd <> BB.word64LE len <> BB.byteString filt
  egvDeserialize cur = parseTillEndOfInput $ do
    blockInfoRecHeaderHash <- fmap BSS.toShort $ Parse.take (getBlockHashLength cur)
    len <- fromIntegral <$> anyWord64le
    blockInfoRecAddressFilter <- Parse.take len
    pure BlockInfoRec{..}
