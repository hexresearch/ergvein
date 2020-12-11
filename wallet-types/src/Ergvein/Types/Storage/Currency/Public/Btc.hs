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

import qualified Network.Haskoin.Block as HB

data BtcPubStorage = BtcPubStorage {
    _btcPubStorage'transactions  :: !(Map BtcTxId BtcTx)
  , _btcPubStorage'utxos         :: !BtcUtxoSet
  , _btcPubStorage'headers       :: !(Map HB.BlockHash HB.BlockHeader)
  , _btcPubStorage'outgoing      :: !(Set BtcTxId)
  , _btcPubStorage'headerSeq     :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  } deriving (Eq, Show, Read)


instance SafeCopy BtcPubStorage where
  version = 1
  putCopy BtcPubStorage{..} = contain $ do
    safePut _btcPubStorage'transactions
    safePut _btcPubStorage'utxos
    put _btcPubStorage'headers
    put _btcPubStorage'outgoing
    put _btcPubStorage'headerSeq
  getCopy = contain $ BtcPubStorage <$> safeGet <*> safeGet <*> get <*> get <*> get

-- This instances is required only for the current version
makeLenses ''BtcPubStorage
