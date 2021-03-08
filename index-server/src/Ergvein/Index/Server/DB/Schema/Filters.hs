{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Filters
  (
    ScannedHeightRecKey(..)
  , ScannedHeightRec(..)
  , BlockInfoRecKey(..)
  , BlockInfoRec(..)
  , initBlockInfoRecTable
  , initScannedHeightTable
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import GHC.Generics
import Data.Serialize (Serialize)

import Database.SQLite.Simple
import Text.InterpolatedString.Perl6 (qc)

import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString.Short   as BSS

-- ===========================================================================
--           Block Info record
-- ===========================================================================

data BlockInfoRecKey = BlockInfoRecKey
  { blockInfoRecKeyCurrency     :: !Currency
  , blockInfoRecKeyBlockHeight  :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord, Serialize)

data BlockInfoRec = BlockInfoRec
  { blockInfoRecHeaderHash     :: !ShortByteString
  , blockInfoRecAddressFilter  :: !ByteString
  } deriving (Generic, Show, Eq, Ord)

initBlockInfoRecTable :: Connection -> IO ()
initBlockInfoRecTable conn = execute_ conn [qc|
    CREATE TABLE IF NOT EXISTS block_info (
      bi_cur INTEGER NOT NULL,
      bi_height INTEGER NOT NULL,
      bi_hash BLOB NOT NULL,
      bi_filt BLOB NOT NULL,
      PRIMARY KEY (bi_cur, bi_height));
  |]

instance FromRow BlockInfoRec where
  fromRow = do
    bh <- field
    filt <- field
    pure $ BlockInfoRec (BSS.toShort bh) filt

-- ===========================================================================
--           Scanned height record
-- ===========================================================================

data ScannedHeightRecKey = ScannedHeightRecKey
  { scannedHeightRecKey      :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data ScannedHeightRec = ScannedHeightRec
  { scannedHeightRecHeight   :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

initScannedHeightTable :: Connection -> IO ()
initScannedHeightTable conn = execute_ conn [qc|
  CREATE TABLE IF NOT EXISTS scanned_height (
    sh_cur INTEGER PRIMARY KEY,
    sh_height INTEGER NOT NULL);
  |]

-- ===========================================================================
--           Queries
-- ===========================================================================
