module Ergvein.Wallet.Monad.Storage
  (
    MonadStorage(..)
  ) where

import Data.Text (Text)
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Native
import Reflex

import qualified Data.Map.Strict as M

class (MonadBaseConstr t m, HasStoreDir m) => MonadStorage t m | m -> t where
  getAddressByCurIx          :: Currency -> Int -> m Base58
  getEncryptedPrivateStorage :: m EncryptedPrivateStorage
  getWalletName              :: m Text
  getPublicKeys              :: m (M.Map Currency EgvPubKeyсhain)
  storeWallet                :: Event t () -> m ()
