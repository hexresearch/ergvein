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
import Ergvein.Types.Transaction
import Network.Haskoin.Keys

import qualified Data.Map.Strict as M

type WalletName = Text

type Password = Text

data CurrencyPrvStorage = CurrencyPrvStorage {
    _currencyPrvStorage'prvKeystore :: PrvKeystore
  }

makeLenses ''CurrencyPrvStorage

$(deriveJSON aesonOptionsStripToApostroph ''CurrencyPrvStorage)

type CurrencyPrvStorages = M.Map Currency CurrencyPrvStorage

data PrvStorage = PrvStorage {
    _prvStorage'seed                :: Seed
  , _prvStorage'rootPrvKey          :: EgvRootXPrvKey
  , _prvStorage'currencyPrvStorages :: CurrencyPrvStorages
  }

makeLenses ''PrvStorage

instance ToJSON PrvStorage where
  toJSON PrvStorage{..} = object [
      "seed"                .= toJSON (bs2Base64Text _prvStorage'seed)
    , "rootPrvKey"          .= toJSON _prvStorage'rootPrvKey
    , "currencyPrvStorages" .= toJSON _prvStorage'currencyPrvStorages
    ]

instance FromJSON PrvStorage where
  parseJSON = withObject "PrvStorage" $ \o -> PrvStorage
    <$> fmap base64Text2bs (o .: "seed")
    <*> o .: "rootPrvKey"
    <*> o .: "currencyPrvStorages"

data EncryptedPrvStorage = EncryptedPrvStorage {
    _encryptedPrvStorage'ciphertext :: ByteString
  , _encryptedPrvStorage'salt       :: ByteString
  , _encryptedPrvStorage'iv         :: IV AES256
  }

makeLenses ''EncryptedPrvStorage

instance ToJSON EncryptedPrvStorage where
  toJSON EncryptedPrvStorage{..} = object [
      "ciphertext" .= toJSON (bs2Base64Text _encryptedPrvStorage'ciphertext)
    , "salt"       .= toJSON (bs2Base64Text _encryptedPrvStorage'salt)
    , "iv"         .= toJSON (bs2Base64Text (convert _encryptedPrvStorage'iv :: ByteString))
    ]

instance FromJSON EncryptedPrvStorage where
  parseJSON = withObject "EncryptedPrvStorage" $ \o -> do
    ciphertext <- fmap base64Text2bs (o .: "ciphertext")
    salt       <- fmap base64Text2bs (o .: "salt")
    iv         <- fmap base64Text2bs (o .: "iv")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> pure $ EncryptedPrvStorage ciphertext salt iv'

data CurrencyPubStorage = CurrencyPubStorage {
    _currencyPubStorage'pubKeystore  :: PubKeystore
  , _currencyPubStorage'transactions :: M.Map TxId EgvTx
  }

makeLenses ''CurrencyPubStorage

$(deriveJSON aesonOptionsStripToApostroph ''CurrencyPubStorage)

type CurrencyPubStorages = M.Map Currency CurrencyPubStorage

data PubStorage = PubStorage {
    _pubStorage'rootPubKey          :: EgvRootXPubKey
  , _pubStorage'currencyPubStorages :: CurrencyPubStorages
  }

makeLenses ''PubStorage

$(deriveJSON aesonOptionsStripToApostroph ''PubStorage)

data WalletStorage = WalletStorage {
    _storage'encryptedPrvStorage :: EncryptedPrvStorage
  , _storage'pubStorage          :: PubStorage
  , _storage'walletName          :: Text
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
