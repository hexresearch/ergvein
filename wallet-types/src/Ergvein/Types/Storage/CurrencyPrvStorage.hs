-- {-# OPTIONS_GHC -Wunused-top-binds #-}
-- Turn on unused-top-binds (if it's off) to see which TH-generated lenses to export
-- Read the README.md to learn now to work with migrations
module Ergvein.Types.Storage.CurrencyPrvStorage
  (
    CurrencyPrvStorage(..)
  , CurrencyPrvStorages
  , currencyPrvStorage'prvKeystore
  , currencyPrvStorage'path
  ) where

import Control.Lens
import Data.Map (Map)
import Data.SafeCopy

import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys.PrvKeystore

data CurrencyPrvStorage = CurrencyPrvStorage {
    _currencyPrvStorage'prvKeystore :: !PrvKeystore
  , _currencyPrvStorage'path        :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)


instance SafeCopy CurrencyPrvStorage where
  version = 1
  putCopy (CurrencyPrvStorage ks p)= contain $ safePut ks >> safePut p
  getCopy = contain $ CurrencyPrvStorage <$> safeGet <*> safeGet

type CurrencyPrvStorages = Map Currency CurrencyPrvStorage

-- This instances is required only for the current version
makeLenses ''CurrencyPrvStorage
