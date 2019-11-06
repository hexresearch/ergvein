module Ergvein.Wallet.Storage.Data
  (
    WalletData(..)
  , EncryptedWalletData(..)
  , ErgveinStorage(..)
  , EncryptedErgveinStorage(..)
  ) where

import Data.Aeson
import Data.ByteArray (convert, Bytes)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Sequence
import Data.Text
import Ergvein.Aeson
import Ergvein.Crypto

import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as MI

data WalletData = WalletData {
    wallet'seed     :: Seed
  , wallet'root     :: EgvRootKey
  , wallet'masters  :: M.Map Currency EgvXPrvKey
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

data EncryptedWalletData = EncryptedWalletData {
    encryptedWallet'ciphertext :: ByteString
  , encryptedWallet'salt       :: ByteString
  , encryptedWallet'iv         :: IV AES256
  }

instance ToJSON EncryptedWalletData where
  toJSON EncryptedWalletData{..} = object [
      "ciphertext" .= toJSON (BS.unpack encryptedWallet'ciphertext)
    , "salt"       .= toJSON (BS.unpack encryptedWallet'salt)
    , "iv"         .= toJSON (BS.unpack (convert encryptedWallet'iv :: ByteString))
    ]

instance FromJSON EncryptedWalletData where
  parseJSON = withObject "EncryptedWalletData" $ \o -> do
    ciphertext <- fmap BS.pack (o .: "ciphertext")
    salt <- fmap BS.pack (o .: "salt")
    iv <- fmap BS.pack (o .: "iv")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> pure $ EncryptedWalletData ciphertext salt iv'

data ErgveinStorage = ErgveinStorage {
    storage'wallet     :: EncryptedWalletData
  , storage'pubKeys    :: M.Map Currency (MI.IntMap Base58)
  , storage'walletName :: Text
  }

instance Eq ErgveinStorage where
  a == b = storage'walletName a == storage'walletName b

$(deriveJSON aesonOptionsStripToApostroph ''ErgveinStorage)

data EncryptedErgveinStorage = EncryptedErgveinStorage {
    encryptedStorage'ciphertext :: ByteString
  , encryptedStorage'salt       :: ByteString
  , encryptedStorage'iv         :: IV AES256
  , encryptedStorage'eciesPoint :: Point Curve_X25519
  , encryptedStorage'authTag    :: AuthTag
  }

instance ToJSON EncryptedErgveinStorage where
  toJSON EncryptedErgveinStorage{..} = object [
      "ciphertext" .= toJSON (BS.unpack encryptedStorage'ciphertext)
    , "salt"       .= toJSON (BS.unpack encryptedStorage'salt)
    , "iv"         .= toJSON (BS.unpack (convert encryptedStorage'iv :: ByteString))
    , "eciesPoint" .= toJSON (BS.unpack eciesPoint)
    , "authTag"    .= toJSON (BS.unpack (convert encryptedStorage'authTag :: ByteString))
    ]
    where
      curve = Proxy :: Proxy Curve_X25519
      eciesPoint = encodePoint curve encryptedStorage'eciesPoint :: ByteString

instance FromJSON EncryptedErgveinStorage where
  parseJSON = withObject "EncryptedErgveinStorage" $ \o -> do
    ciphertext <- fmap BS.pack (o .: "ciphertext")
    salt <- fmap BS.pack (o .: "salt")
    iv <- fmap BS.pack (o .: "iv")
    eciesPoint <- fmap BS.pack (o .: "eciesPoint")
    authTag <- fmap BS.pack (o .: "authTag")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> case decodePoint curve eciesPoint of
        CryptoFailed _ -> fail "failed to read eciesPoint"
        CryptoPassed eciesPoint' -> pure $ EncryptedErgveinStorage ciphertext salt iv' eciesPoint' authTag'
        where 
          curve = Proxy :: Proxy Curve_X25519
          authTag' = AuthTag (convert authTag :: Bytes)
