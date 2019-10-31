module Ergvein.Wallet.Storage.Secure.Data
  (
    WalletData(..)
  , EncryptedWalletData(..)
  ) where

import Ergvein.Aeson
import Ergvein.Crypto
import Data.Sequence
import Data.Text

data WalletData = WalletData
  { mnemonic    :: Mnemonic
  , privateKeys :: Seq Base58
  } deriving (Show)
$(deriveJSON defaultOptions ''WalletData)

data EncryptedWalletData = EncryptedWalletData
  { encryptedData   :: Text
  , salt            :: Text
  , initVector      :: Text
  } deriving (Show)
$(deriveJSON defaultOptions ''EncryptedWalletData)
