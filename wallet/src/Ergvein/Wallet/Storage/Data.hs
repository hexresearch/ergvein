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

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as B64
import qualified Data.IntMap.Strict       as MI
import qualified Data.Map.Strict          as M
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Encoding       as TE

byteStringToText :: ByteString -> Text
byteStringToText bs = TE.decodeUtf8With TEE.lenientDecode $ B64.encode bs

textToByteString :: Text -> ByteString
textToByteString = B64.decodeLenient . TE.encodeUtf8

data WalletData = WalletData {
    wallet'seed     :: Seed
  , wallet'root     :: EgvRootKey
  , wallet'masters  :: M.Map Currency EgvXPrvKey
  }

instance ToJSON WalletData where
  toJSON WalletData{..} = object [
      "seed"    .= toJSON (byteStringToText wallet'seed)
    , "root"    .= toJSON wallet'root
    , "masters" .= toJSON wallet'masters
    ]

instance FromJSON WalletData where
  parseJSON = withObject "WalletData" $ \o -> WalletData
    <$> fmap textToByteString (o .: "seed")
    <*> o .: "root"
    <*> o .: "masters"

data EncryptedWalletData = EncryptedWalletData {
    encryptedWallet'ciphertext :: ByteString
  , encryptedWallet'salt       :: ByteString
  , encryptedWallet'iv         :: IV AES256
  }

instance ToJSON EncryptedWalletData where
  toJSON EncryptedWalletData{..} = object [
      "ciphertext" .= toJSON (byteStringToText encryptedWallet'ciphertext)
    , "salt"       .= toJSON (byteStringToText encryptedWallet'salt)
    , "iv"         .= toJSON (byteStringToText (convert encryptedWallet'iv :: ByteString))
    ]

instance FromJSON EncryptedWalletData where
  parseJSON = withObject "EncryptedWalletData" $ \o -> do
    ciphertext <- fmap textToByteString (o .: "ciphertext")
    salt       <- fmap textToByteString (o .: "salt")
    iv         <- fmap textToByteString (o .: "iv")
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
      "ciphertext" .= toJSON (byteStringToText encryptedStorage'ciphertext)
    , "salt"       .= toJSON (byteStringToText encryptedStorage'salt)
    , "iv"         .= toJSON (byteStringToText (convert encryptedStorage'iv :: ByteString))
    , "eciesPoint" .= toJSON (byteStringToText eciesPoint)
    , "authTag"    .= toJSON (byteStringToText (convert encryptedStorage'authTag :: ByteString))
    ]
    where
      curve = Proxy :: Proxy Curve_X25519
      eciesPoint = encodePoint curve encryptedStorage'eciesPoint :: ByteString

instance FromJSON EncryptedErgveinStorage where
  parseJSON = withObject "EncryptedErgveinStorage" $ \o -> do
    ciphertext <- fmap textToByteString (o .: "ciphertext")
    salt <- fmap textToByteString (o .: "salt")
    iv <- fmap textToByteString (o .: "iv")
    eciesPoint <- fmap textToByteString (o .: "eciesPoint")
    authTag <- fmap textToByteString (o .: "authTag")
    case makeIV iv of
      Nothing -> fail "failed to read iv"
      Just iv' -> case decodePoint curve eciesPoint of
        CryptoFailed _ -> fail "failed to read eciesPoint"
        CryptoPassed eciesPoint' -> pure $ EncryptedErgveinStorage ciphertext salt iv' eciesPoint' authTag'
        where 
          curve = Proxy :: Proxy Curve_X25519
          authTag' = AuthTag (convert authTag :: Bytes)
