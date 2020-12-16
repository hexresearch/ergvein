module Ergvein.Index.Server.DB.Serialize
  (
    EgvSerialize(..)
  , putTxInfosAsRecs
  , serializeWord32
  , deserializeWord32
  ) where

import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Builder as BB
import Data.Word

import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString.Lazy as BL
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

putTxInfosAsRecs :: Currency -> BlockHeight -> [TxInfo] -> LDB.WriteBatch
putTxInfosAsRecs cur bheight infos = mconcat $ parMap rpar putI (force infos)
  where
    putI TxInfo{..} = [
        LDB.Put (txBytesKey txHash) $ egvSerialize cur $ TxRecBytes txBytes
      , LDB.Put (txHeightKey txHash) $ egvSerialize cur $ TxRecHeight $ fromIntegral bheight
      , LDB.Put (txUnspentKey txHash) $ egvSerialize cur $ TxRecUnspent txOutputsCount
      ]
