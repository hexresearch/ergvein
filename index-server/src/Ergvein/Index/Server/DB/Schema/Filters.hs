{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Filters
  (
    ScannedHeightRecKey(..)
  , ScannedHeightRec(..)
  , BlockMetaRecKey(..)
  , BlockMetaRec(..)
  , TxRecBytes(..)
  , TxRecMeta(..)
  , TxRawKey(..)
  , txRawKey
  , txMetaKey
  , scannedHeightTxKey
  , metaRecKey
  , unPrefixedKey
  , schemaVersionRecKey
  , schemaVersion
  ) where

import Crypto.Hash.SHA256
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.FileEmbed
import GHC.Generics
import Data.Serialize (Serialize)
import Data.Word

import Ergvein.Index.Server.DB.Utils
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString as BS
import qualified Data.Serialize as S

data KeyPrefix = ScannedHeight | Meta | TxRaw | TxMeta | SchemaVersion deriving Enum

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

txRawKey :: TxHash -> ByteString
txRawKey = keyString TxRaw . TxRawKey
{-# INLINE txRawKey #-}

txMetaKey :: TxHash -> ByteString
txMetaKey = keyString TxMeta . TxRawKey
{-# INLINE txMetaKey #-}

data TxRawKey = TxRawKey {unTxRawKey :: !TxHash }
  deriving (Generic, Show, Eq, Ord, Serialize)

data TxRecBytes = TxRecBytes { unTxRecBytes :: !ByteString }
  deriving (Generic, Show, Eq, Ord)

data TxRecMeta = TxRecMeta { unTxRecMeta :: !Word32 }
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
