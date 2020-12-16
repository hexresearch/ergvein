{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Filters
  (
    ScannedHeightRecKey(..)
  , ScannedHeightRec(..)
  , BlockMetaRecKey(..)
  , BlockMetaRec(..)
  , TxRecBytes(..)
  -- , TxRecMeta(..)
  , TxRecHeight(..)
  , TxRecUnspent(..)
  , TxBytesKey(..)
  , txBytesKey
  , txHeightKey
  , txUnspentKey
  , scannedHeightTxKey
  , metaRecKey
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
import Data.Word

import Ergvein.Index.Server.DB.Utils
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Short   as BSS
import qualified Data.Serialize          as S
import qualified Data.ByteString.Builder as BB

data KeyPrefix
  = SchemaVersion
  | ScannedHeight
  | Meta
  | TxBytes
  | TxHeight
  | TxUnspent
  deriving Enum

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Filters.hs")

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

--ScannedHeight

scannedHeightTxKey :: Currency -> ByteString
scannedHeightTxKey = keyString ScannedHeight . ScannedHeightRecKey

data ScannedHeightRecKey = ScannedHeightRecKey
  { scannedHeightRecKey      :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data ScannedHeightRec = ScannedHeightRec
  { scannedHeightRecHeight   :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

--Tx

txBytesKey :: TxHash -> ByteString
txBytesKey = keyString TxBytes . TxBytesKey
{-# INLINE txBytesKey #-}

txHeightKey :: TxHash -> ByteString
txHeightKey = keyString TxHeight . TxBytesKey
{-# INLINE txHeightKey #-}

txUnspentKey :: TxHash -> ByteString
txUnspentKey = keyString TxUnspent . TxBytesKey
{-# INLINE txUnspentKey #-}

data TxBytesKey = TxBytesKey {unTxBytesKey :: !TxHash }
  deriving (Generic, Show, Eq, Ord, Serialize)

data TxRecBytes = TxRecBytes { unTxRecBytes :: !ByteString }
  deriving (Generic, Show, Eq, Ord)

data TxRecHeight = TxRecHeight { unTxRecHeight :: !Word32 }
  deriving (Generic, Show, Eq, Ord)

data TxRecUnspent = TxRecUnspent { unTxRecUnspent :: !Word32 }
  deriving (Generic, Show, Eq, Ord)

--BlockMeta

metaRecKey :: (Currency, BlockHeight) -> ByteString
metaRecKey = keyString Meta . uncurry BlockMetaRecKey

data BlockMetaRecKey = BlockMetaRecKey
  { blockMetaRecKeyCurrency     :: !Currency
  , blockMetaRecKeyBlockHeight  :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord, Serialize)

data BlockMetaRec = BlockMetaRec
  { blockMetaRecHeaderHash     :: !ShortByteString
  , blockMetaRecAddressFilter  :: !ByteString
  } deriving (Generic, Show, Eq, Ord)

--SchemaVersion

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

data SchemaVersionRec = Text  deriving (Generic, Show, Eq, Ord)


-- ===========================================================================
--           instances EgvSerialize
-- ===========================================================================

instance EgvSerialize ScannedHeightRec where
  egvSerialize _ (ScannedHeightRec sh) = BL.toStrict . BB.toLazyByteString $ BB.word64LE sh
  egvDeserialize _ = parseOnly $ ScannedHeightRec <$> anyWord64le

instance EgvSerialize TxRecBytes where
  egvSerialize _ = BL.toStrict . BB.toLazyByteString . buildBS . unTxRecBytes
  egvDeserialize _ = fmap TxRecBytes . parseOnly parseBS

instance EgvSerialize TxRecHeight where
  egvSerialize _ = BL.toStrict . BB.toLazyByteString . BB.word32LE . unTxRecHeight
  egvDeserialize _ = fmap TxRecHeight . parseOnly anyWord32le

instance EgvSerialize TxRecUnspent where
  egvSerialize _ = BL.toStrict . BB.toLazyByteString . BB.word32LE . unTxRecUnspent
  egvDeserialize _ = fmap TxRecUnspent . parseOnly anyWord32le

instance EgvSerialize BlockMetaRec where
  egvSerialize _ (BlockMetaRec hd filt) = BL.toStrict . BB.toLazyByteString $ let
    len = fromIntegral $ BS.length filt
    in BB.shortByteString hd <> BB.word64LE len <> BB.byteString filt
  egvDeserialize cur = parseOnly $ do
    blockMetaRecHeaderHash <- fmap BSS.toShort $ Parse.take (getBlockHashLength cur)
    len <- fromIntegral <$> anyWord64le
    blockMetaRecAddressFilter <- Parse.take len
    pure BlockMetaRec{..}
