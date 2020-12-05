-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
module Ergvein.Types.Storage.CurrencyPubStorage
  (
    CurrencyPubStorage(..)
  , CurrencyPubStorages
  -- * Export lenses
  , currencyPubStorage'pubKeystore
  , currencyPubStorage'path
  , currencyPubStorage'transactions
  , currencyPubStorage'utxos
  , currencyPubStorage'headers
  , currencyPubStorage'outgoing
  , currencyPubStorage'headerSeq
  , currencyPubStorage'scannedHeight
  , currencyPubStorage'chainHeight
  ) where

import Control.Lens
import Data.Map (Map)
import Data.SafeCopy
import Data.Set(Set)
import Data.Vector(Vector)
import Data.Serialize
import Data.Word

import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys.PubKeystore
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo

import qualified Network.Haskoin.Block as HB

data CurrencyPubStorage = CurrencyPubStorage {
    _currencyPubStorage'pubKeystore   :: !PubKeystore
  , _currencyPubStorage'path          :: !(Maybe DerivPrefix)
  , _currencyPubStorage'transactions  :: !(Map TxId EgvTx)
  , _currencyPubStorage'utxos         :: !BtcUtxoSet              -- ^ TODO: Change to a generalized one, after we switch to DMaps
  , _currencyPubStorage'headers       :: !(Map HB.BlockHash HB.BlockHeader)
  , _currencyPubStorage'outgoing      :: !(Set TxId)
  , _currencyPubStorage'headerSeq     :: !(Word32, Vector (HB.BlockHeight, HB.BlockHash))
  , _currencyPubStorage'scannedHeight :: !BlockHeight
  , _currencyPubStorage'chainHeight   :: !BlockHeight
  } deriving (Eq, Show, Read)

makeLenses ''CurrencyPubStorage

instance SafeCopy CurrencyPubStorage where
  version = 1
  putCopy CurrencyPubStorage{..} = contain $ do
    safePut _currencyPubStorage'pubKeystore
    safePut _currencyPubStorage'path
    safePut _currencyPubStorage'transactions
    put _currencyPubStorage'utxos
    put _currencyPubStorage'headers
    put _currencyPubStorage'outgoing
    put _currencyPubStorage'headerSeq
    put _currencyPubStorage'scannedHeight
    put _currencyPubStorage'chainHeight
  getCopy = contain $ CurrencyPubStorage
    <$> safeGet <*> safeGet <*> safeGet <*> get
    <*> get <*> get <*> get <*> get <*> get

type CurrencyPubStorages = Map Currency CurrencyPubStorage
