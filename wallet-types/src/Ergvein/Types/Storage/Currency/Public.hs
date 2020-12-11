-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Ergvein.Types.Storage.Currency.Public
  (
    CurrencyPubStorage(..)
  , PubStorageMeta(..)
  , CurrencyPubStorages
  , _currencyPubStorage'outgoing
  , _currencyPubStorage'transactions
  -- * Export lenses
  , currencyPubStorage'pubKeystore
  , currencyPubStorage'path
  , currencyPubStorage'transactions
  , currencyPubStorage'outgoing
  , currencyPubStorage'scannedHeight
  , currencyPubStorage'chainHeight
  , currencyPubStorage'meta
  , _PubStorageBtc
  , _PubStorageErgo
  ) where

import Control.Lens
import Data.Map (Map)
import Data.Maybe
import Data.SafeCopy
import Data.Serialize
import Data.Set (Set)

import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys.Store.Public
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Storage.Currency.Public.Ergo
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data CurrencyPubStorage = CurrencyPubStorage {
    _currencyPubStorage'pubKeystore   :: !PubKeystore
  , _currencyPubStorage'path          :: !(Maybe DerivPrefix)
  , _currencyPubStorage'scannedHeight :: !BlockHeight
  , _currencyPubStorage'chainHeight   :: !BlockHeight
  , _currencyPubStorage'meta          :: !PubStorageMeta
  } deriving (Eq, Show, Read)


instance SafeCopy CurrencyPubStorage where
  version = 1
  putCopy CurrencyPubStorage{..} = contain $ do
    safePut _currencyPubStorage'pubKeystore
    safePut _currencyPubStorage'path
    put _currencyPubStorage'scannedHeight
    put _currencyPubStorage'chainHeight
    safePut _currencyPubStorage'meta
  getCopy = contain $ CurrencyPubStorage
    <$> safeGet <*> safeGet <*> get <*> get <*> safeGet

data PubStorageMeta = PubStorageBtc !BtcPubStorage | PubStorageErgo !ErgoPubStorage
  deriving (Eq, Show, Read)

instance SafeCopy PubStorageMeta where
  version = 1
  putCopy v = contain $ case v of
    PubStorageBtc  a -> put (0 :: Int) >> safePut a
    PubStorageErgo a -> put (1 :: Int) >> safePut a
  getCopy = contain $ do
    i :: Int <- get
    case i of
      0 -> PubStorageBtc <$> safeGet
      1 -> PubStorageErgo <$> safeGet
      _ -> fail $ "Unknown PubStorageMeta tag " <> show i

type CurrencyPubStorages = Map Currency CurrencyPubStorage

-- This instances is required only for the current version
makeLenses ''CurrencyPubStorage
makePrisms ''PubStorageMeta

_currencyPubStorage'transactions :: CurrencyPubStorage -> Map TxId EgvTx
_currencyPubStorage'transactions CurrencyPubStorage{..} = case _currencyPubStorage'meta of
  PubStorageBtc bs  -> mbimap BtcTxHash TxBtc . _btcPubStorage'transactions $ bs
  PubStorageErgo es -> mbimap ErgTxHash TxErg . _ergoPubStorage'transactions $ es

mbimap :: (Ord k2) => (k1 -> k2) -> (a -> b) -> M.Map k1 a -> M.Map k2 b
mbimap fk fe = M.map fe . M.mapKeys fk

currencyPubStorage'transactions :: Lens' CurrencyPubStorage (Map TxId EgvTx)
currencyPubStorage'transactions = lens _currencyPubStorage'transactions $ \s txs -> s {
    _currencyPubStorage'meta = case _currencyPubStorage'meta s of
      PubStorageBtc bs  -> PubStorageBtc bs {
          _btcPubStorage'transactions = mbimap (unwrap . toBtcTxHash) (unwrap . toTxBtc) txs
        }
      PubStorageErgo bs  -> PubStorageErgo bs {
          _ergoPubStorage'transactions = mbimap (unwrap . toErgTxHash) (unwrap . toTxErg) txs
        }
    }
  where
    unwrap = fromMaybe (error "currencyPubStorage'transactions: transaction types doesn't match!")

_currencyPubStorage'outgoing :: CurrencyPubStorage -> Set TxId
_currencyPubStorage'outgoing CurrencyPubStorage{..} = case _currencyPubStorage'meta of
  PubStorageBtc bs  -> S.map BtcTxHash . _btcPubStorage'outgoing $ bs
  PubStorageErgo es -> S.map ErgTxHash . _ergoPubStorage'outgoing $ es

currencyPubStorage'outgoing :: Lens' CurrencyPubStorage (Set TxId)
currencyPubStorage'outgoing = lens _currencyPubStorage'outgoing $ \s txs -> s {
    _currencyPubStorage'meta = case _currencyPubStorage'meta s of
      PubStorageBtc bs  -> PubStorageBtc bs {
          _btcPubStorage'outgoing = S.map  (unwrap . toBtcTxHash) txs
        }
      PubStorageErgo bs  -> PubStorageErgo bs {
          _ergoPubStorage'outgoing = S.map  (unwrap . toErgTxHash) txs
        }
    }
  where
    unwrap = fromMaybe (error "currencyPubStorage'outgoing: transaction types doesn't match!")
