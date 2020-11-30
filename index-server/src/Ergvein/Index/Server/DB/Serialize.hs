module Ergvein.Index.Server.DB.Serialize
  (
    EgvSerialize(..)
  , putTxInfosAsRecs
  , serializeWord32
  , deserializeWord32
  ) where

import Control.DeepSeq
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Builder as BB
import Data.Foldable
import Data.Word

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Serialize as S
import qualified Data.Text.Encoding as TE
import qualified Database.LevelDB as LDB


-- ===========================================================================
--           Some utils
-- ===========================================================================

serializeWord32 :: Word32 -> ByteString
serializeWord32 = BL.toStrict . toLazyByteString . word32LE
{-# INLINE serializeWord32 #-}

deserializeWord32 :: ByteString -> Either String Word32
deserializeWord32 = parseOnly anyWord32le
{-# INLINE deserializeWord32 #-}

putTxInfosAsRecs :: Currency -> [TxInfo] -> LDB.WriteBatch
putTxInfosAsRecs cur items = mconcat $ parMap rpar putI (force items)
  where
    putI TxInfo{..} = let
      p1 = LDB.Put (txRawKey txHash) $ egvSerialize cur $ TxRecBytes txBytes
      p2 = LDB.Put (txMetaKey txHash) $ egvSerialize cur $ TxRecMeta txOutputsCount
      in [p1, p2]

