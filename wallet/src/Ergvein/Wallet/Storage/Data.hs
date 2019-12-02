module Ergvein.Wallet.Storage.Data
  (
    PrivateStorage(..)
  , EncryptedPrivateStorage(..)
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

data PrivateStorage = PrivateStorage {
    privateStorage'seed :: Seed
  , privateStorage'root :: XPrvKey
  , privateStorage'keys :: M.Map Currency EgvPrvKeyсhain
  }

instance ToJSON PrivateStorage where
  toJSON PrivateStorage{..} = object [
      "seed" .= toJSON (BS.unpack privateStorage'seed)
    , "root" .= toJSON privateStorage'root
    , "keys" .= toJSON privateStorage'keys
    ]

instance FromJSON PrivateStorage where
  parseJSON = withObject "PrivateStorage" $ \o -> PrivateStorage
    <$> fmap BS.pack (o .: "seed")
    <*> o .: "root"
    <*> o .: "keys"

data EncryptedPrivateStorage = EncryptedPrivateStorage {
    encryptedPrivateStorage'ciphertext :: ByteString
  , encryptedPrivateStorage'salt       :: ByteString
  , encryptedPrivateStorage'iv         :: IV AES256
  }

instance ToJSON EncryptedPrivateStorage where
  toJSON EncryptedPrivateStorage{..} = object [
      "ciphertext" .= toJSON (BS.unpack encryptedPrivateStorage'ciphertext)
    , "salt"       .= toJSON (BS.unpack encryptedPrivateStorage'salt)
    , "iv"         .= toJSON (BS.unpack (convert encryptedPrivateStorage'iv :: ByteString))
    ]

instance FromJSON EncryptedPrivateStorage where
  parseJSON = withObject "EncryptedPrivateStorage" $ \o -> do
    ciphertext <- fmap BS.pack (o .: "ciphertext")
    salt <- fmap BS.pack (o .: "salt")
    iv <- fmap BS.pack (o .: "iv")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> pure $ EncryptedPrivateStorage ciphertext salt iv'

data ErgveinStorage = ErgveinStorage {
    storage'encryptedPrivateStorage :: EncryptedPrivateStorage
  , storage'publicStorage           :: M.Map Currency (MI.IntMap Base58)
  , storage'walletName              :: Text
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
