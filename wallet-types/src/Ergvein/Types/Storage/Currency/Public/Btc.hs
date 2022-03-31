-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Ergvein.Types.Storage.Currency.Public.Btc
  (
    BtcPubStorage(..)
  , defBtcAddrs
  -- * Export lenses
  , btcPubStorage'transactions
  , btcPubStorage'utxos
  , btcPubStorage'headers
  , btcPubStorage'outgoing
  , btcPubStorage'headerSeq
  , btcPubStorage'replacedTxs
  , btcPubStorage'possiblyReplacedTxs
  , btcPubStorage'restoreStartHeight
  , btcPubStorage'preferredNodes
  , btcPubStorage'customNode
  ) where

import Control.Lens
import Data.Map (Map)
import Data.SafeCopy
import Data.Serialize
import Data.Set(Set)
import Data.Text (Text)
import Data.Vector(Vector)
import Data.Word

import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Btc

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.Haskoin.Block as HB

-- | I had to put this somewhere
defBtcAddrs :: Bool -> [Text]
defBtcAddrs isTestnet = if isTestnet
  then []
  else [ "79.143.71.50:8333"
       , "188.244.4.78:8333"
       ]

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

data BtcPubStorage_V2 = BtcPubStorage_V2 {
    _btcPubStorageV2'transactions        :: !(Map BtcTxId BtcTx)
  , _btcPubStorageV2'utxos               :: !BtcUtxoSet
  , _btcPubStorageV2'headers             :: !(Map HB.BlockHash HB.BlockHeader)
  , _btcPubStorageV2'outgoing            :: !(Set BtcTxId)
  , _btcPubStorageV2'headerSeq           :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  , _btcPubStorageV2'replacedTxs         :: !(Map BtcTxId (S.Set BtcTxId)) -- ^ Stores history of tx replacements by fee.
  , _btcPubStorageV2'possiblyReplacedTxs :: !(Map BtcTxId (S.Set BtcTxId))
    -- ^ Stores sequences of unconfirmed RBF transactions,
    -- for which we cannot determine the tx with highest fee (replacing transaction).
    -- Map keys are the most recent transactions received by the wallet,
    -- so we assume they have the highest fees and will replace others.
    -- Note: Map values does not contain replacing tx.
    -- TODO: probably it is better to use Set (TxId, (S.Set TxId)) instead of
    -- M.Map TxId (S.Set TxId) here.
  } deriving (Eq, Show, Read)

instance SafeCopy BtcPubStorage_V2 where
  version = 2
  putCopy BtcPubStorage_V2{..} = contain $ do
    safePut _btcPubStorageV2'transactions
    safePut _btcPubStorageV2'utxos
    put _btcPubStorageV2'headers
    put _btcPubStorageV2'outgoing
    put _btcPubStorageV2'headerSeq
    put _btcPubStorageV2'replacedTxs
    put _btcPubStorageV2'possiblyReplacedTxs
  getCopy = contain $ BtcPubStorage_V2 <$> safeGet <*> safeGet <*> get <*> get <*> get <*> get <*> get
  kind = extension

instance Migrate BtcPubStorage_V2 where
  type MigrateFrom BtcPubStorage_V2 = BtcPubStorage_V1
  migrate (BtcPubStorage_V1 a b c d e) = BtcPubStorage_V2 a b c d e M.empty M.empty

data BtcPubStorage_V3 = BtcPubStorage_V3 {
    _btcPubStorageV3'transactions        :: !(Map BtcTxId BtcTx)
  , _btcPubStorageV3'utxos               :: !BtcUtxoSet
  , _btcPubStorageV3'headers             :: !(Map HB.BlockHash HB.BlockHeader)
  , _btcPubStorageV3'outgoing            :: !(Set BtcTxId)
  , _btcPubStorageV3'headerSeq           :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  , _btcPubStorageV3'replacedTxs         :: !(Map BtcTxId (S.Set BtcTxId)) -- ^ Stores history of tx replacements by fee.
  , _btcPubStorageV3'possiblyReplacedTxs :: !(Map BtcTxId (S.Set BtcTxId))
    -- ^ Stores sequences of unconfirmed RBF transactions,
    -- for which we cannot determine the tx with highest fee (replacing transaction).
    -- Map keys are the most recent transactions received by the wallet,
    -- so we assume they have the highest fees and will replace others.
    -- Note: Map values does not contain replacing tx.
    -- TODO: probably it is better to use Set (TxId, (S.Set TxId)) instead of
    -- M.Map TxId (S.Set TxId) here.
  , _btcPubStorageV3'restoreStartHeight :: !(Maybe Word64)
  } deriving (Eq, Show, Read)

instance SafeCopy BtcPubStorage_V3 where
  version = 3
  putCopy BtcPubStorage_V3{..} = contain $ do
    safePut _btcPubStorageV3'transactions
    safePut _btcPubStorageV3'utxos
    put _btcPubStorageV3'headers
    put _btcPubStorageV3'outgoing
    put _btcPubStorageV3'headerSeq
    put _btcPubStorageV3'replacedTxs
    put _btcPubStorageV3'possiblyReplacedTxs
    put _btcPubStorageV3'restoreStartHeight
  getCopy = contain $ BtcPubStorage_V3 <$> safeGet <*> safeGet <*> get <*> get <*> get <*> get <*> get <*> get
  kind = extension

instance Migrate BtcPubStorage_V3 where
  type MigrateFrom BtcPubStorage_V3 = BtcPubStorage_V2
  migrate (BtcPubStorage_V2 a b c d e f g) = BtcPubStorage_V3 a b c d e f g Nothing

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
  , _btcPubStorage'restoreStartHeight :: !(Maybe Word64)
  -- | Preferred btc node adresses
  , _btcPubStorage'preferredNodes     :: !(Set Text)
  -- | Custom BTC node. Maybe
  , _btcPubStorage'customNode         :: !(Maybe Text)
  } deriving (Eq, Show, Read)

instance SafeCopy BtcPubStorage where
  version = 4
  putCopy BtcPubStorage{..} = contain $ do
    safePut _btcPubStorage'transactions
    safePut _btcPubStorage'utxos
    put _btcPubStorage'headers
    put _btcPubStorage'outgoing
    put _btcPubStorage'headerSeq
    put _btcPubStorage'replacedTxs
    put _btcPubStorage'possiblyReplacedTxs
    put _btcPubStorage'restoreStartHeight
    put _btcPubStorage'preferredNodes
    put _btcPubStorage'customNode
  getCopy = contain $ BtcPubStorage <$> safeGet <*> safeGet <*> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get
  kind = extension

instance Migrate BtcPubStorage where
  type MigrateFrom BtcPubStorage = BtcPubStorage_V3
  migrate (BtcPubStorage_V3 a b c d e f g h) = BtcPubStorage a b c d e f g h (S.fromList $ defBtcAddrs False) Nothing

-- This instances is required only for the current version
makeLenses ''BtcPubStorage
