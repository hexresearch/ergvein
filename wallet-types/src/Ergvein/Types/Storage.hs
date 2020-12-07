{-# LANGUAGE ScopedTypeVariables #-}
module Ergvein.Types.Storage
  (
    WalletName
  , Password
  , pubStorageKeys
  , pubStoragePubMaster
  , pubStorageLastUnusedKey
  , pubStorageKeyStorage
  , pubStorageSetKeyStorage
  , modifyCurrStorage
  , pubStorageTxs
  -- * Reexport latest general version
  -- along with specific other versions
  , module Reexport
  ) where

import Data.Text
import Data.Vector (Vector)

import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Orphanage ()
import Ergvein.Types.Storage.Currency.Private as Reexport
import Ergvein.Types.Storage.Currency.Public as Reexport
import Ergvein.Types.Storage.Private as Reexport
import Ergvein.Types.Storage.Public as Reexport
import Ergvein.Types.Storage.Wallet as Reexport
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M

type WalletName = Text

type Password = Text

-- ====================================================================
--      Utils
-- ====================================================================

-- | Get pub storage keys
pubStorageKeys :: Currency -> KeyPurpose -> PubStorage -> Vector EgvXPubKey
pubStorageKeys c kp = fmap pubKeyBox'key . maybe mempty keys . fmap _currencyPubStorage'pubKeystore . M.lookup c . _pubStorage'currencyPubStorages
  where
    keys = case kp of
      External -> pubKeystore'external
      Internal -> pubKeystore'internal

pubStoragePubMaster :: Currency -> PubStorage -> Maybe EgvXPubKey
pubStoragePubMaster c = fmap pubKeystore'master . pubStorageKeyStorage c

pubStorageLastUnusedKey :: Currency -> KeyPurpose -> PubStorage -> Maybe (Int, EgvPubKeyBox)
pubStorageLastUnusedKey c kp ps = getLastUnusedKey kp . _currencyPubStorage'pubKeystore =<< M.lookup c (_pubStorage'currencyPubStorages ps)

pubStorageKeyStorage :: Currency -> PubStorage -> Maybe PubKeystore
pubStorageKeyStorage c = fmap _currencyPubStorage'pubKeystore . M.lookup c . _pubStorage'currencyPubStorages

pubStorageSetKeyStorage :: Currency -> PubKeystore -> PubStorage -> PubStorage
pubStorageSetKeyStorage c ks ps = ps {
    _pubStorage'currencyPubStorages = M.adjust f c $ _pubStorage'currencyPubStorages ps
  }
  where
    f cps = cps {
        _currencyPubStorage'pubKeystore = ks
      }

modifyCurrStorage :: Currency -> (CurrencyPubStorage  -> CurrencyPubStorage) -> PubStorage -> PubStorage
modifyCurrStorage c f ps = ps {
    _pubStorage'currencyPubStorages = M.adjust f c $ _pubStorage'currencyPubStorages ps
  }

pubStorageTxs :: Currency -> PubStorage -> Maybe (M.Map TxId EgvTx)
pubStorageTxs c = fmap _currencyPubStorage'transactions . M.lookup c . _pubStorage'currencyPubStorages
