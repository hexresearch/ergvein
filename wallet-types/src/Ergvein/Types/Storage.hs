module Ergvein.Types.Storage
  (
    PrivateStorage(..)
  , EncryptedPrivateStorage(..)
  , ErgveinStorage(..)
  , EncryptedErgveinStorage(..)
  ) where

import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.ECC (Curve_X25519, Point, encodePoint, decodePoint)
import Crypto.Error
import Data.Aeson
import Data.ByteArray (convert, Bytes)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.Sequence
import Data.Text
import Ergvein.Aeson
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Network.Haskoin.Keys

import qualified Data.ByteString.Base64   as B64
import qualified Data.IntMap.Strict       as MI
import qualified Data.Map.Strict          as M
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Encoding       as TE

data PrivateStorage = PrivateStorage {
    privateStorage'seed        :: Seed
  , privateStorage'root        :: EgvRootPrvKey
  , privateStorage'privateKeys :: M.Map Currency EgvPrvKeyсhain
  }

instance ToJSON PrivateStorage where
  toJSON PrivateStorage{..} = object [
      "seed"        .= toJSON (bs2Base64Text privateStorage'seed)
    , "root"        .= toJSON privateStorage'root
    , "privateKeys" .= toJSON privateStorage'privateKeys
    ]

instance FromJSON PrivateStorage where
  parseJSON = withObject "PrivateStorage" $ \o -> PrivateStorage
    <$> fmap base64Text2bs (o .: "seed")
    <*> o .: "root"
    <*> o .: "privateKeys"

data EncryptedPrivateStorage = EncryptedPrivateStorage {
    encryptedPrivateStorage'ciphertext :: ByteString
  , encryptedPrivateStorage'salt       :: ByteString
  , encryptedPrivateStorage'iv         :: IV AES256
  }

instance ToJSON EncryptedPrivateStorage where
  toJSON EncryptedPrivateStorage{..} = object [
      "ciphertext" .= toJSON (bs2Base64Text encryptedPrivateStorage'ciphertext)
    , "salt"       .= toJSON (bs2Base64Text encryptedPrivateStorage'salt)
    , "iv"         .= toJSON (bs2Base64Text (convert encryptedPrivateStorage'iv :: ByteString))
    ]

instance FromJSON EncryptedPrivateStorage where
  parseJSON = withObject "EncryptedPrivateStorage" $ \o -> do
    ciphertext <- fmap base64Text2bs (o .: "ciphertext")
    salt       <- fmap base64Text2bs (o .: "salt")
    iv         <- fmap base64Text2bs (o .: "iv")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> pure $ EncryptedPrivateStorage ciphertext salt iv'

data ErgveinStorage = ErgveinStorage {
    storage'encryptedPrivateStorage :: EncryptedPrivateStorage
  , storage'publicKeys              :: M.Map Currency EgvPubKeyсhain
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
      "ciphertext" .= toJSON (bs2Base64Text encryptedStorage'ciphertext)
    , "salt"       .= toJSON (bs2Base64Text encryptedStorage'salt)
    , "iv"         .= toJSON (bs2Base64Text (convert encryptedStorage'iv :: ByteString))
    , "eciesPoint" .= toJSON (bs2Base64Text eciesPoint)
    , "authTag"    .= toJSON (bs2Base64Text (convert encryptedStorage'authTag :: ByteString))
    ]
    where
      curve = Proxy :: Proxy Curve_X25519
      eciesPoint = encodePoint curve encryptedStorage'eciesPoint :: ByteString

instance FromJSON EncryptedErgveinStorage where
  parseJSON = withObject "EncryptedErgveinStorage" $ \o -> do
    ciphertext <- fmap base64Text2bs (o .: "ciphertext")
    salt <- fmap base64Text2bs (o .: "salt")
    iv <- fmap base64Text2bs (o .: "iv")
    eciesPoint <- fmap base64Text2bs (o .: "eciesPoint")
    authTag <- fmap base64Text2bs (o .: "authTag")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> case decodePoint curve eciesPoint of
        CryptoFailed _ -> fail "failed to read eciesPoint"
        CryptoPassed eciesPoint' -> pure $ EncryptedErgveinStorage ciphertext salt iv' eciesPoint' authTag'
        where 
          curve = Proxy :: Proxy Curve_X25519
          authTag' = AuthTag (convert authTag :: Bytes)
