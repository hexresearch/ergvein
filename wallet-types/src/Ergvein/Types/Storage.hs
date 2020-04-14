module Ergvein.Types.Storage where

import Control.Lens (makeLenses)
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.ECC (Curve_X25519, Point, encodePoint, decodePoint)
import Crypto.Error
import Data.Aeson
import Data.ByteArray (convert, Bytes)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Text
import Ergvein.Aeson
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Network.Haskoin.Keys

import qualified Data.Map.Strict as M

type WalletName = Text

type Password = Text

type PrivateKeystore = M.Map Currency EgvPrvKeyсhain

data PrivateStorage = PrivateStorage {
    _privateStorage'seed        :: Seed
  , _privateStorage'root        :: EgvRootXPrvKey
  , _privateStorage'privateKeys :: PrivateKeystore
  }

makeLenses ''PrivateStorage

instance ToJSON PrivateStorage where
  toJSON PrivateStorage{..} = object [
      "seed"        .= toJSON (bs2Base64Text _privateStorage'seed)
    , "root"        .= toJSON _privateStorage'root
    , "privateKeys" .= toJSON _privateStorage'privateKeys
    ]

instance FromJSON PrivateStorage where
  parseJSON = withObject "PrivateStorage" $ \o -> PrivateStorage
    <$> fmap base64Text2bs (o .: "seed")
    <*> o .: "root"
    <*> o .: "privateKeys"

data EncryptedPrivateStorage = EncryptedPrivateStorage {
    _encryptedPrivateStorage'ciphertext :: ByteString
  , _encryptedPrivateStorage'salt       :: ByteString
  , _encryptedPrivateStorage'iv         :: IV AES256
  }

makeLenses ''EncryptedPrivateStorage

instance ToJSON EncryptedPrivateStorage where
  toJSON EncryptedPrivateStorage{..} = object [
      "ciphertext" .= toJSON (bs2Base64Text _encryptedPrivateStorage'ciphertext)
    , "salt"       .= toJSON (bs2Base64Text _encryptedPrivateStorage'salt)
    , "iv"         .= toJSON (bs2Base64Text (convert _encryptedPrivateStorage'iv :: ByteString))
    ]

instance FromJSON EncryptedPrivateStorage where
  parseJSON = withObject "EncryptedPrivateStorage" $ \o -> do
    ciphertext <- fmap base64Text2bs (o .: "ciphertext")
    salt       <- fmap base64Text2bs (o .: "salt")
    iv         <- fmap base64Text2bs (o .: "iv")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> pure $ EncryptedPrivateStorage ciphertext salt iv'

type PublicStorage = M.Map Currency EgvPubKeyсhain

data WalletStorage = WalletStorage {
    _storage'encryptedPrivateStorage :: EncryptedPrivateStorage
  , _storage'publicKeys              :: PublicStorage
  , _storage'walletName              :: Text
  }

makeLenses ''WalletStorage

instance Eq WalletStorage where
  a == b = _storage'walletName a == _storage'walletName b

$(deriveJSON aesonOptionsStripToApostroph ''WalletStorage)

data EncryptedWalletStorage = EncryptedWalletStorage {
    _encryptedStorage'ciphertext :: ByteString
  , _encryptedStorage'salt       :: ByteString
  , _encryptedStorage'iv         :: IV AES256
  , _encryptedStorage'eciesPoint :: Point Curve_X25519
  , _encryptedStorage'authTag    :: AuthTag
  }

makeLenses ''EncryptedWalletStorage

instance ToJSON EncryptedWalletStorage where
  toJSON EncryptedWalletStorage{..} = object [
      "ciphertext" .= toJSON (bs2Base64Text _encryptedStorage'ciphertext)
    , "salt"       .= toJSON (bs2Base64Text _encryptedStorage'salt)
    , "iv"         .= toJSON (bs2Base64Text (convert _encryptedStorage'iv :: ByteString))
    , "eciesPoint" .= toJSON (bs2Base64Text eciesPoint)
    , "authTag"    .= toJSON (bs2Base64Text (convert _encryptedStorage'authTag :: ByteString))
    ]
    where
      curve = Proxy :: Proxy Curve_X25519
      eciesPoint = encodePoint curve _encryptedStorage'eciesPoint :: ByteString

instance FromJSON EncryptedWalletStorage where
  parseJSON = withObject "EncryptedWalletStorage" $ \o -> do
    ciphertext <- fmap base64Text2bs (o .: "ciphertext")
    salt <- fmap base64Text2bs (o .: "salt")
    iv <- fmap base64Text2bs (o .: "iv")
    eciesPoint <- fmap base64Text2bs (o .: "eciesPoint")
    authTag <- fmap base64Text2bs (o .: "authTag")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> case decodePoint curve eciesPoint of
        CryptoFailed _ -> fail "failed to read eciesPoint"
        CryptoPassed eciesPoint' -> pure $ EncryptedWalletStorage ciphertext salt iv' eciesPoint' authTag'
        where
          curve = Proxy :: Proxy Curve_X25519
          authTag' = AuthTag (convert authTag :: Bytes)
