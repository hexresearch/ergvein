-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Ergvein.Types.Storage.Currency.Public.Btc
  (
    BtcPubStorage(..)
  -- * Export lenses
  , btcPubStorage'transactions
  , btcPubStorage'utxos
  , btcPubStorage'headers
  , btcPubStorage'outgoing
  , btcPubStorage'headerSeq
  , btcPubStorage'replacedTxs
  , btcPubStorage'possiblyReplacedTxs
  ) where

import Control.Lens
import Data.Map (Map)
import Data.SafeCopy
import Data.Set(Set)
import Data.Vector(Vector)
import Data.Serialize
import Data.Word

import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Btc

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.Haskoin.Block as HB

data BtcPubStorage_V1 = BtcPubStorage_V1 {
    _btcPubStorageV1'transactions  :: !(Map BtcTxId BtcTx)
  , _btcPubStorageV1'utxos         :: !BtcUtxoSet
  , _btcPubStorageV1'headers       :: !(Map HB.BlockHash HB.BlockHeader)
  , _btcPubStorageV1'outgoing      :: !(Set BtcTxId)
  , _btcPubStorageV1'headerSeq     :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  } deriving (Eq, Show, Read)

instance SafeCopy BtcPubStorage_V1 where
  version = 1
  putCopy BtcPubStorage_V1{..} = contain $ do
    safePut _btcPubStorageV1'transactions
    safePut _btcPubStorageV1'utxos
    put _btcPubStorageV1'headers
    put _btcPubStorageV1'outgoing
    put _btcPubStorageV1'headerSeq
  getCopy = contain $ BtcPubStorage_V1 <$> safeGet <*> safeGet <*> get <*> get <*> get

data BtcPubStorage = BtcPubStorage {
    _btcPubStorage'transactions        :: !(Map BtcTxId BtcTx)
  , _btcPubStorage'utxos               :: !BtcUtxoSet
  , _btcPubStorage'headers             :: !(Map HB.BlockHash HB.BlockHeader)
  , _btcPubStorage'outgoing            :: !(Set BtcTxId)
  , _btcPubStorage'headerSeq           :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  , _btcPubStorage'replacedTxs         :: !(Map BtcTxId (S.Set BtcTxId)) -- ^ Stores history of tx replacements by fee.
  , _btcPubStorage'possiblyReplacedTxs :: !(Map BtcTxId (S.Set BtcTxId))
    -- ^ Stores sequences of unconfirmed RBF transactions,
    -- for which we cannot determine the tx with highest fee (replacing transaction).
    -- Map keys are the most recent transactions received by the wallet,
    -- so we assume they have the highest fees and will replace others.
    -- Note: Map values does not contain replacing tx.
    -- TODO: probably it is better to use Set (TxId, (S.Set TxId)) instead of
    -- M.Map TxId (S.Set TxId) here.
  } deriving (Eq, Show, Read)

instance SafeCopy BtcPubStorage where
  version = 2
  putCopy BtcPubStorage{..} = contain $ do
    safePut _btcPubStorage'transactions
    safePut _btcPubStorage'utxos
    put _btcPubStorage'headers
    put _btcPubStorage'outgoing
    put _btcPubStorage'headerSeq
    put _btcPubStorage'replacedTxs
    put _btcPubStorage'possiblyReplacedTxs
  getCopy = contain $ BtcPubStorage <$> safeGet <*> safeGet <*> get <*> get <*> get <*> get <*> get

instance Migrate BtcPubStorage where
  type MigrateFrom BtcPubStorage = BtcPubStorage_V1
  migrate (BtcPubStorage_V1 a b c d e) = BtcPubStorage a b c d e M.empty M.empty

-- This instances is required only for the current version
makeLenses ''BtcPubStorage
