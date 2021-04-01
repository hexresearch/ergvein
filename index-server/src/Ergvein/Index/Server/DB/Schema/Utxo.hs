{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Utxo
  (
    TxRecBytes(..)
  , TxRecHeight(..)
  , TxRecUnspent(..)
  , TxBytesKey(..)
  , txBytesKey
  , txHeightKey
  , txUnspentKey
  , schemaVersion
  , schemaVersionRecKey
  ) where

import Crypto.Hash.SHA256
import Data.Attoparsec.Binary
import Data.ByteString (ByteString)
import Data.FileEmbed
import GHC.Generics
import Data.Serialize (Serialize)
import Data.Word

import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Transaction

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.Serialize          as S
import qualified Data.ByteString.Builder as BB
import Ergvein.Index.Protocol.Utils

data KeyPrefix
  = SchemaVersion
  | TxHeight
  | TxBytes
  | TxUnspent
  deriving Enum

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

-- ===========================================================================
--           Schema Version
-- ===========================================================================

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Utxo.hs")

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

-- ===========================================================================
--           Tx records
-- ===========================================================================

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

-- ===========================================================================
--           instances EgvSerialize
-- ===========================================================================

instance EgvSerialize TxRecBytes where
  egvSerialize _ = BL.toStrict . BB.toLazyByteString . buildBS . unTxRecBytes
  egvDeserialize _ = fmap TxRecBytes . parseTillEndOfInput parseBS

instance EgvSerialize TxRecHeight where
  egvSerialize _ = BL.toStrict . BB.toLazyByteString . BB.word32LE . unTxRecHeight
  egvDeserialize _ = fmap TxRecHeight . parseTillEndOfInput anyWord32le

instance EgvSerialize TxRecUnspent where
  egvSerialize _ = BL.toStrict . BB.toLazyByteString . BB.word32LE . unTxRecUnspent
  egvDeserialize _ = fmap TxRecUnspent . parseTillEndOfInput anyWord32le
