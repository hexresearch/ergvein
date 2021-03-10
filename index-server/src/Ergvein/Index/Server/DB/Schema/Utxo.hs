{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Utxo
  (
    TxRecBytes(..)
  , TxRecHeight(..)
  , TxRecUnspent(..)
  , TxBytesKey(..)
  , ScannedHeightRecKey(..)
  , ScannedHeightRec(..)
  , initTxHeightTable
  , initTxLastHeightTable
  , initUtxoTable
  , initScanProgresTable
  ) where

import Data.ByteString (ByteString)
import Data.Serialize (Serialize)
import Data.Word
import Database.SQLite.Simple
import GHC.Generics
import Text.InterpolatedString.Perl6 (qc)

import Ergvein.Types.Transaction
import Ergvein.Types.Currency

-- ===========================================================================
--           Tx records
-- ===========================================================================

data TxBytesKey = TxBytesKey {unTxBytesKey :: !TxHash }
  deriving (Generic, Show, Eq, Ord, Serialize)

data TxRecBytes = TxRecBytes { unTxRecBytes :: !ByteString }
  deriving (Generic, Show, Eq, Ord)

data TxRecHeight = TxRecHeight { unTxRecHeight :: !Word32 }
  deriving (Generic, Show, Eq, Ord)

data TxRecUnspent = TxRecUnspent { unTxRecUnspent :: !Word32 }
  deriving (Generic, Show, Eq, Ord)

initTxHeightTable :: Connection -> IO ()
initTxHeightTable conn = execute_ conn [qc|
    CREATE TABLE IF NOT EXISTS tx_height (
      th_hash BLOB PRIMARY KEY,
      th_height INTEGER NOT NULL);
  |]

initTxLastHeightTable :: Connection -> IO ()
initTxLastHeightTable conn = execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS tx_last_height(
    tlh_cur INTEGER PRIMARY KEY,
    tlh_height INTEGER NOT NULL);
|]

initUtxoTable :: Connection -> IO ()
initUtxoTable conn = execute_ conn [qc|
    CREATE TABLE IF NOT EXISTS utxo (
      utxo_txhash BLOB PRIMARY KEY,
      utxo_txraw BLOB NOT NULL,
      utxo_txunspent INT NOT NULL);
  |]

-- ===========================================================================
--           Scanned height record
-- ===========================================================================

data ScannedHeightRecKey = ScannedHeightRecKey
  { scannedHeightRecKey      :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data ScannedHeightRec = ScannedHeightRec
  { scannedHeightRecHeight   :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)


initScanProgresTable :: Connection -> IO ()
initScanProgresTable conn = execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS scan_progress (
    sp_cur INTEGER PRIMARY KEY,
    sp_height INTEGER NOT NULL,
    sp_blk_hash BLOB NOT NULL,
    sp_last_hash BLOB NOT NULL
    );
  |]
