module Ergvein.Wallet.Storage.Data
  (
    WalletData(..)
  , EncryptedWalletData(..)
  , ErgveinStorage(..)
  ) where

import Ergvein.Aeson
import Ergvein.Crypto
import Data.Sequence
import Data.Text

import qualified Data.Map.Strict as M

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

data ErgveinStorage = ErgveinStorage{
  storage'wallet     :: EncryptedWalletData
, storage'pubKeys    :: M.Map EgvXPubKey [Base58]
, storage'walletName :: Text
}

instance Eq ErgveinStorage where
  a == b = storage'walletName a == storage'walletName b

$(deriveJSON aesonOptionsStripToApostroph ''ErgveinStorage)
