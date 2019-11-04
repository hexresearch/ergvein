module Ergvein.Wallet.Storage.Data
  (
    WalletData(..)
  , EncryptedWalletData(..)
  , ErgveinStorage(..)
  ) where

import Data.Aeson
import Ergvein.Aeson
import Ergvein.Crypto
import Data.Sequence
import Data.Text

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

data WalletData = WalletData
  { wallet'seed     :: Seed
  , wallet'root     :: EgvRootKey
  , wallet'masters  :: M.Map NetworkTag EgvXPrvKey
  }

instance ToJSON WalletData where
  toJSON WalletData{..} = object [
      "seed"    .= toJSON (BS.unpack wallet'seed)
    , "root"    .= toJSON wallet'root
    , "masters" .= toJSON wallet'masters
    ]

instance FromJSON WalletData where
  parseJSON = withObject "WalletData" $ \o -> WalletData
    <$> fmap BS.pack (o .: "seed")
    <*> o .: "root"
    <*> o .: "masters"

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
