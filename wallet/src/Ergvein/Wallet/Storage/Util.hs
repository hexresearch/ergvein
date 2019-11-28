module Ergvein.Wallet.Storage.Util(
    WalletData(..)
  , EncryptedWalletData(..)
  , WalletName
  , encryptWalletData
  , decryptWalletData
  , passwordToECIESPrvKey
  , createWallet
  , createStorage
  , storageFilePrefix
  , saveStorageToFile
  , loadStorageFromFile
  ) where

import Control.Monad.IO.Class
import Data.ByteArray           (convert)
import Data.ByteString          (ByteString)
import Data.ByteString.Base64   (encode, decodeLenient)
import Data.Proxy
import Data.Sequence
import Data.Text                (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Ergvein.Aeson
import Ergvein.Crypto
import Ergvein.IO
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Native
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Data
import System.Directory
import System.FilePath

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS

type Password = Text
type WalletName = Text

createWallet :: Mnemonic -> Either StorageAlerts WalletData
createWallet mnemonic = case mnemonicToSeed "" mnemonic of
  Left err -> Left $ SAMnemonicFail $ showt err
  Right seed -> let
    root = makeXPrvKey seed
    masters = M.fromList $ fmap (\c -> (c, deriveCurrencyKey root c)) allCurrencies
    in Right $ WalletData seed (EgvRootKey root) masters

createStorage :: MonadIO m => Mnemonic -> (WalletName, Password) -> m (Either StorageAlerts ErgveinStorage)
createStorage mnemonic (login, pass) = either (pure . Left) (\wd -> do
  ewdResult <- encryptWalletData wd pass
  case ewdResult of
    Left err -> pure $ Left err
    Right ewd -> pure $ Right $ ErgveinStorage ewd mempty login) $ createWallet mnemonic

encryptWalletData :: MonadIO m => WalletData -> Password -> m (Either StorageAlerts EncryptedWalletData)
encryptWalletData walletData password = liftIO $ do
  salt :: ByteString <- genRandomSalt
  let secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
  iv <- genRandomIV (undefined :: AES256)
  case iv of
    Nothing -> pure $ Left $ SACryptoError "Failed to generate an AES initialization vector"
    Just iv' -> do
      let walletDataBS = encodeUtf8 $ encodeJson walletData
      case encrypt secretKey iv' walletDataBS of
        Left err -> pure $ Left $ SACryptoError $ showt err
        Right eData -> pure $ Right $ EncryptedWalletData {
            encryptedWallet'ciphertext = eData
          , encryptedWallet'salt       = salt
          , encryptedWallet'iv         = iv'
          }

decryptWalletData :: EncryptedWalletData -> Password -> Either StorageAlerts WalletData
decryptWalletData encryptedWalletData password =
  case decrypt secretKey iv ciphertext of
    Left err -> Left $ SACryptoError $ showt err
    Right decryptedData -> do
      let walletData = decodeJson $ decodeUtf8With lenientDecode decryptedData
      case walletData of
        Left err -> Left $ SACryptoError $ showt err
        Right wd -> Right wd
  where
    salt = encryptedWallet'salt encryptedWalletData
    secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
    iv = encryptedWallet'iv encryptedWalletData
    ciphertext = encryptedWallet'ciphertext encryptedWalletData

encryptStorage :: (MonadIO m, MonadRandom m) => ErgveinStorage -> ECIESPubKey -> m (Either StorageAlerts EncryptedErgveinStorage)
encryptStorage storage publicKey = do
  let curve = Proxy :: Proxy Curve_X25519
  deriveEncryptResult <- deriveEncrypt curve publicKey
  case deriveEncryptResult of
    CryptoFailed err -> pure $ Left $ SACryptoError $ showt err
    CryptoPassed (eciesPoint, sharedSecret) -> do
      salt :: ByteString <- genRandomSalt
      let secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params sharedSecret salt) :: Key AES256 ByteString
      iv' <- genRandomIV (undefined :: AES256)
      case iv' of
        Nothing -> pure $ Left $ SACryptoError "Failed to generate an initialization vector"
        Just iv -> do
          let storageBS = encodeUtf8 $ encodeJson storage
              ivBS = convert iv :: ByteString
              eciesPointBS = encodePoint curve eciesPoint :: ByteString
              encryptedData = encryptWithAEAD AEAD_GCM secretKey iv (BS.concat [salt, ivBS, eciesPointBS]) storageBS defaultAuthTagLength
          case encryptedData of
            Left err -> pure $ Left $ SACryptoError $ showt err
            Right (authTag, ciphertext) -> pure $ Right $ EncryptedErgveinStorage {
                encryptedStorage'ciphertext = ciphertext
              , encryptedStorage'salt       = salt
              , encryptedStorage'iv         = iv
              , encryptedStorage'eciesPoint = eciesPoint
              , encryptedStorage'authTag    = authTag
              }

decryptStorage :: EncryptedErgveinStorage -> ECIESPrvKey -> Either StorageAlerts ErgveinStorage
decryptStorage encryptedStorage privateKey = do
  let curve = Proxy :: Proxy Curve_X25519
      ciphertext = encryptedStorage'ciphertext encryptedStorage
      salt       = encryptedStorage'salt       encryptedStorage
      iv         = encryptedStorage'iv         encryptedStorage
      eciesPoint = encryptedStorage'eciesPoint encryptedStorage
      authTag    = encryptedStorage'authTag    encryptedStorage
  case deriveDecrypt curve eciesPoint privateKey of
      CryptoFailed err -> Left $ SACryptoError $ showt err
      CryptoPassed sharedSecret -> do
        let ivBS = convert iv :: ByteString
            eciesPointBS = encodePoint curve eciesPoint :: ByteString
            secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params sharedSecret salt) :: Key AES256 ByteString
            decryptedData = decryptWithAEAD AEAD_GCM secretKey iv (BS.concat [salt, ivBS, eciesPointBS]) ciphertext authTag
        case decryptedData of
          Nothing -> Left $ SACryptoError "Failed to decrypt storage"
          Just decryptedStorage -> case storage of
            Left err -> Left $ SACryptoError $ showt err
            Right s -> Right s
            where
              storage = decodeJson $ decodeUtf8With lenientDecode decryptedStorage

passwordToECIESPrvKey :: Password -> Either StorageAlerts ECIESPrvKey
passwordToECIESPrvKey password = case secretKey passwordHash of
  CryptoFailed err -> Left $ SACryptoError "Failed to generate an ECIES secret key from password"
  CryptoPassed key -> Right key
  where
    passwordHash = fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) BS.empty :: ByteString

storageFilePrefix :: Text
storageFilePrefix = "wallet_"

saveStorageToFile :: (MonadIO m, MonadRandom m, HasStoreDir m, PlatformNatives)
  => ECIESPubKey -> ErgveinStorage -> m ()
saveStorageToFile publicKey storage = do
  let fname = storageFilePrefix <> T.replace " " "_" (storage'walletName storage)
  encryptedStorage <- encryptStorage storage publicKey
  case encryptedStorage of
    Left _ -> fail "Failed to encrypt storage"
    Right encStorage -> storeValue fname encStorage

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => WalletName -> Password -> m (Either StorageAlerts ErgveinStorage)
loadStorageFromFile login pass = do
  let fname = storageFilePrefix <> T.replace " " "_" login
  storageResp <- readStoredFile fname
  case storageResp of
    Left err -> pure $ Left $ SANativeAlert err
    Right storageText -> case decodeJson $ T.concat storageText of
      Left err -> pure $ Left $ SADecodeError err
      Right storage -> case passwordToECIESPrvKey pass of
        Left err -> pure $ Left err
        Right pubKey -> case decryptStorage storage pubKey of
          Left err -> pure $ Left err
          Right s -> pure $ Right s

-- Alerts regarding secure storage system
data StorageAlerts
  = SADecodeError Text
  | SALoadedSucc
  | SANativeAlert NativeAlerts
  | SAMnemonicFail Text
  | SACryptoError Text
  deriving (Eq)

instance LocalizedPrint StorageAlerts where
  localizedShow l v = case l of
    English -> case v of
      SADecodeError e -> "Storage loading error: " <> e
      SALoadedSucc    -> "Storage loaded"
      SANativeAlert a -> localizedShow l a
      SAMnemonicFail t -> "Failed to produce seed from mnemonic: " <> t
      SACryptoError e -> "Cryptographic error: " <> e
    Russian -> localizedShow English v
