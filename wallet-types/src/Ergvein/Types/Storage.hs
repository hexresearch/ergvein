{-# LANGUAGE ScopedTypeVariables #-}
module Ergvein.Types.Storage where

import Control.Lens (makeLenses, (&), (%~))
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.ECC (Curve_X25519, Point, encodePoint, decodePoint)
import Crypto.Error
import Data.Aeson
import Data.ByteArray (convert, Bytes)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text
import Data.Vector (Vector)
import Data.Word
import Network.Haskoin.Keys
import qualified Network.Haskoin.Block as HB

import Ergvein.Aeson
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

type WalletName = Text

type Password = Text

data CurrencyPrvStorage = CurrencyPrvStorage {
    _currencyPrvStorage'prvKeystore :: !PrvKeystore
  , _currencyPrvStorage'path        :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

makeLenses ''CurrencyPrvStorage

$(deriveJSON aesonOptionsStripToApostroph ''CurrencyPrvStorage)

type CurrencyPrvStorages = M.Map Currency CurrencyPrvStorage

data PrvStorage = PrvStorage {
    _prvStorage'mnemonic            :: Mnemonic
  , _prvStorage'rootPrvKey          :: EgvRootXPrvKey
  , _prvStorage'currencyPrvStorages :: CurrencyPrvStorages
  , _prvStorage'pathPrefix          :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

makeLenses ''PrvStorage

instance ToJSON PrvStorage where
  toJSON PrvStorage{..} = object [
      "mnemonic"            .= toJSON _prvStorage'mnemonic
    , "rootPrvKey"          .= toJSON _prvStorage'rootPrvKey
    , "currencyPrvStorages" .= toJSON _prvStorage'currencyPrvStorages
    , "pathPrefix"          .= toJSON _prvStorage'pathPrefix
    ]

instance FromJSON PrvStorage where
  parseJSON = withObject "PrvStorage" $ \o -> PrvStorage
    <$> o .: "mnemonic"
    <*> o .: "rootPrvKey"
    <*> o .: "currencyPrvStorages"
    <*> o .:? "pathPrefix" .!= Just legacyDerivPathPrefix

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
    _currencyPubStorage'pubKeystore   :: !PubKeystore
  , _currencyPubStorage'path          :: !(Maybe DerivPrefix)
  , _currencyPubStorage'transactions  :: !(M.Map TxId EgvTx)
  , _currencyPubStorage'utxos         :: !BtcUtxoSet              -- ^ TODO: Change to a generalized one, after we switch to DMaps
  , _currencyPubStorage'headers       :: !(M.Map HB.BlockHash HB.BlockHeader)
  , _currencyPubStorage'outgoing      :: !(S.Set TxId)
  , _currencyPubStorage'headerSeq     :: !(Word32, V.Vector (HB.BlockHeight, HB.BlockHash))
  , _currencyPubStorage'scannedHeight :: !BlockHeight
  , _currencyPubStorage'chainHeight   :: !BlockHeight
  } deriving (Eq, Show, Read)

makeLenses ''CurrencyPubStorage

$(deriveJSON aesonOptionsStripToApostroph ''CurrencyPubStorage)

instance FromJSONKey HB.BlockHash where
instance ToJSONKey HB.BlockHash where

type CurrencyPubStorages = M.Map Currency CurrencyPubStorage

data PubStorage = PubStorage {
    _pubStorage'rootPubKey          :: !EgvRootXPubKey
  , _pubStorage'currencyPubStorages :: !CurrencyPubStorages
  , _pubStorage'activeCurrencies    :: [Currency]
  , _pubStorage'restoring           :: !Bool -- ^ Flag to track unfinished process of restoration
  , _pubStorage'pathPrefix          :: !(Maybe DerivPrefix)
  } deriving (Eq, Show, Read)

makeLenses ''PubStorage

$(deriveJSON aesonOptionsStripToApostroph ''PubStorage)

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
