module Ergvein.Index.Server.DB.Monad
  (
    HasDbs(..)
  , dbConfig
  , dbColumns
  , scannedHeightKey
  , lastScannedBlockHashKey
  ) where

import Control.Monad.Reader
import Data.ByteString
import Data.Default
import Database.RocksDB

import Ergvein.Types.Currency

class Monad m => HasDbs m where
  getDb :: m DB
  getUtxoCF :: Currency -> m ColumnFamily
  getFiltersCF :: Currency -> m ColumnFamily
  getMetaCF :: Currency -> m ColumnFamily

type DbEnv = (DB, ColumnFamily, ColumnFamily, ColumnFamily)

instance MonadIO m => HasDbs (ReaderT DbEnv m) where
  getDb = asks $ \(db,_,_,_) -> db
  getUtxoCF _ = asks $ \(_,cf,_,_) -> cf
  getFiltersCF _ = asks $ \(_,_,cf,_) -> cf
  getMetaCF _ = asks $ \(_,_,_,cf) -> cf

dbConfig :: Config
dbConfig = def {createIfMissing = True}

dbColumns :: [(String, Config)]
dbColumns = [
    ("btc_utxo", def {bloomFilter = True})
  , ("btc_filters", def)
  , ("btc_meta", def)
  ]


-- BTC:
--   UTXO:
--     Key :: OutPoint              : ByteString + Word32
--     Value :: ByteString
--   Filters:
--     Key :: BlockHeight           : VarInt
--     Value :: HeaderHash + Filter : ByteString + ByteString
--   Meta:
--     Key :: Different keys for different fields
--     Value :: Different values. See below


-- Value :: BlockHeight (VarInt)
scannedHeightKey :: ByteString
scannedHeightKey = "scannedHeightKey"

-- Value :: ByteString
lastScannedBlockHashKey :: ByteString
lastScannedBlockHashKey = "lastScannedBlockHashKey"
