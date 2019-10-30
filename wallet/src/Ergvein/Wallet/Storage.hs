module Ergvein.Wallet.Storage
  (
    ErgveinStorage(..)
  ) where

import Network.Haskoin.Keys
import Network.Haskoin.Address

import qualified Data.Map.Strict as M

import Ergvein.Aeson
import Ergvein.Crypto
import Ergvein.Wallet.Storage.Secure.Data

data ErgveinStorage = ErgveinStorage{
  storageWallet   :: EncryptedWalletData
, storagePubKeys  :: M.Map EgvXPubKey [Base58]
}

$(deriveJSON defaultOptions ''ErgveinStorage)
