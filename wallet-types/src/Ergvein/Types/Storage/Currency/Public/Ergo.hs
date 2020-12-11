-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Ergvein.Types.Storage.Currency.Public.Ergo
  (
    ErgoPubStorage(..)
  -- * Export lenses
  , ergoPubStorage'transactions
  , ergoPubStorage'utxos
  , ergoPubStorage'headers
  , ergoPubStorage'outgoing
  , ergoPubStorage'headerSeq
  ) where

import Control.Lens
import Data.Map (Map)
import Data.SafeCopy
import Data.Set(Set)
import Data.Vector(Vector)
import Data.Serialize
import Data.Word

import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Ergo

import qualified Network.Haskoin.Block as HB

data ErgoPubStorage = ErgoPubStorage {
    _ergoPubStorage'transactions  :: !(Map ErgTxId ErgTx)
  , _ergoPubStorage'utxos         :: !ErgoUtxoSet
  , _ergoPubStorage'headers       :: !(Map HB.BlockHash HB.BlockHeader)
  , _ergoPubStorage'outgoing      :: !(Set ErgTxId)
  , _ergoPubStorage'headerSeq     :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  } deriving (Eq, Show, Read)


instance SafeCopy ErgoPubStorage where
  version = 1
  putCopy ErgoPubStorage{..} = contain $ do
    safePut _ergoPubStorage'transactions
    safePut _ergoPubStorage'utxos
    put _ergoPubStorage'headers
    put _ergoPubStorage'outgoing
    put _ergoPubStorage'headerSeq
  getCopy = contain $ ErgoPubStorage <$> safeGet <*> safeGet <*> get <*> get <*> get

-- This instances is required only for the current version
makeLenses ''ErgoPubStorage
