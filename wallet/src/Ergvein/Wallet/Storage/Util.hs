module Ergvein.Wallet.Storage.Util(
    addXPrvKeyToKeyсhain
  , addXPubKeyToKeyсhain
  , encryptPrivateStorage
  , decryptPrivateStorage
  , passwordToECIESPrvKey
  , createPrivateStorage
  , createStorage
  , storageFilePrefix
  , saveStorageToFile
  , loadStorageFromFile
  , listStorages
  , getLastStorage
  , setLastStorage
  ) where

import Control.Monad.IO.Class
import Data.ByteArray           (convert)
import Data.ByteString          (ByteString)
import Data.Maybe
import Data.Proxy
import Data.Text                (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Ergvein.Aeson
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Localization.Native
import Ergvein.Wallet.Localization.Storage
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as MI
import qualified Data.Map.Strict as M
import qualified Data.Text as T

addXPrvKeyToKeyсhain :: KeyPurpose -> (Int, EgvXPrvKey) -> EgvPrvKeyсhain -> EgvPrvKeyсhain
addXPrvKeyToKeyсhain External (index, key) (EgvPrvKeyсhain master external internal) =
   EgvPrvKeyсhain master (MI.insert index key external) internal
addXPrvKeyToKeyсhain Internal (index, key) (EgvPrvKeyсhain master external internal) =
   EgvPrvKeyсhain master external (MI.insert index key internal)

createPrvKeychain :: EgvRootXPrvKey -> Currency -> EgvPrvKeyсhain
createPrvKeychain root currency =
  let masterKey = deriveCurrencyMasterPrvKey root currency
      externalKeys = MI.fromList [(index, derivePrvKey masterKey External (fromIntegral index)) | index <- [0..(gapLimit-1)]]
      internalKeys = MI.fromList [(index, derivePrvKey masterKey Internal (fromIntegral index)) | index <- [0..(gapLimit-1)]]
  in EgvPrvKeyсhain masterKey externalKeys internalKeys

createPrivateStorage :: Seed -> EgvRootXPrvKey -> PrivateStorage
createPrivateStorage seed root = PrivateStorage seed root privateKeys
  where privateKeys = M.fromList [(currency, createPrvKeychain root currency) | currency <- allCurrencies]

addXPubKeyToKeyсhain :: KeyPurpose -> (Int, EgvXPubKey) -> EgvPubKeyсhain -> EgvPubKeyсhain
addXPubKeyToKeyсhain External (index, key) (EgvPubKeyсhain master external internal) =
  EgvPubKeyсhain master (MI.insert index key external) internal
addXPubKeyToKeyсhain Internal (index, key) (EgvPubKeyсhain master external internal) =
  EgvPubKeyсhain master external (MI.insert index key internal)

createPubKeyсhain :: EgvRootXPrvKey -> Currency -> EgvPubKeyсhain
createPubKeyсhain root currency =
  let masterKey = deriveCurrencyMasterPubKey root currency
      externalKeys = MI.fromList [(index, derivePubKey masterKey External (fromIntegral index)) | index <- [0..(gapLimit-1)]]
      internalKeys = MI.fromList [(index, derivePubKey masterKey Internal (fromIntegral index)) | index <- [0..(gapLimit-1)]]
  in EgvPubKeyсhain masterKey externalKeys internalKeys

createPublicKeystore :: EgvRootXPrvKey -> PublicKeystore
createPublicKeystore root = M.fromList [(currency, createPubKeyсhain root currency) | currency <- allCurrencies]

createStorage :: MonadIO m => Mnemonic -> (WalletName, Password) -> m (Either StorageAlert ErgveinStorage)
createStorage mnemonic (login, pass) = case mnemonicToSeed "" mnemonic of
  Left err -> pure $ Left $ SAMnemonicFail $ showt err
  Right seed -> do
    let root = EgvRootXPrvKey $ makeXPrvKey seed
        privateStorage = createPrivateStorage seed root
        publicKeystore = createPublicKeystore root
    encryptedPrivateStorageResult <- encryptPrivateStorage privateStorage pass
    case encryptedPrivateStorageResult of
      Left err -> pure $ Left err
      Right eps -> pure $ Right $ ErgveinStorage eps publicKeystore login

encryptPrivateStorage :: MonadIO m => PrivateStorage -> Password -> m (Either StorageAlert EncryptedPrivateStorage)
encryptPrivateStorage privateStorage password = liftIO $ do
  salt :: ByteString <- genRandomSalt
  let secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
  iv <- genRandomIV (undefined :: AES256)
  case iv of
    Nothing -> pure $ Left $ SACryptoError "Failed to generate an AES initialization vector"
    Just iv' -> do
      let privateStorageBS = encodeUtf8 $ encodeJson privateStorage
      case encrypt secretKey iv' privateStorageBS of
        Left err -> pure $ Left $ SACryptoError $ showt err
        Right ciphertext -> pure $ Right $ EncryptedPrivateStorage ciphertext salt iv'

decryptPrivateStorage :: EncryptedPrivateStorage -> Password -> Either StorageAlert PrivateStorage
decryptPrivateStorage encryptedPrivateStorage password =
  case decrypt secretKey iv ciphertext of
    Left err -> Left $ SACryptoError $ showt err
    Right decryptedPrivateStorage -> do
      let decodedPrivateStorage = decodeJson $ decodeUtf8With lenientDecode decryptedPrivateStorage
      case decodedPrivateStorage of
        Left err -> Left $ SACryptoError $ showt err
        Right dps -> Right dps
  where
    salt = _encryptedPrivateStorage'salt encryptedPrivateStorage
    secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
    iv = _encryptedPrivateStorage'iv encryptedPrivateStorage
    ciphertext = _encryptedPrivateStorage'ciphertext encryptedPrivateStorage

encryptStorage :: (MonadIO m, MonadRandom m) => ErgveinStorage -> ECIESPubKey -> m (Either StorageAlert EncryptedErgveinStorage)
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
            Right (authTag, ciphertext) -> pure $ Right $ EncryptedErgveinStorage ciphertext salt iv eciesPoint authTag

decryptStorage :: EncryptedErgveinStorage -> ECIESPrvKey -> Either StorageAlert ErgveinStorage
decryptStorage encryptedStorage privateKey = do
  let curve = Proxy :: Proxy Curve_X25519
      ciphertext = _encryptedStorage'ciphertext encryptedStorage
      salt       = _encryptedStorage'salt       encryptedStorage
      iv         = _encryptedStorage'iv         encryptedStorage
      eciesPoint = _encryptedStorage'eciesPoint encryptedStorage
      authTag    = _encryptedStorage'authTag    encryptedStorage
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

passwordToECIESPrvKey :: Password -> Either StorageAlert ECIESPrvKey
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
  let fname = storageFilePrefix <> T.replace " " "_" (_storage'walletName storage)
  logWrite $ "Storing storage to the " <> fname
  encryptedStorage <- encryptStorage storage publicKey
  case encryptedStorage of
    Left _ -> fail "Failed to encrypt storage"
    Right encStorage -> storeValue fname encStorage

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => WalletName -> Password -> m (Either StorageAlert ErgveinStorage)
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

-- | Scan storage folder for all wallets
listStorages :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => m [WalletName]
listStorages = do
  ns <- listKeys
  pure $ catMaybes . fmap isWallet $ ns
  where
    isWallet n = let
      (a, _) = T.breakOn storageFilePrefix n
      in if T.null a then Just (T.drop (T.length storageFilePrefix) n) else Nothing

-- | Name of file where last wallet name is stored
lastWalletFile :: Text
lastWalletFile = ".last-wallet"

-- | Try to check `.last-wallet` file to get name of wallet
getLastStorage :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => m (Maybe WalletName)
getLastStorage = do
  logWrite $ "Reading last storage file " <> lastWalletFile
  mres <- retrieveValue lastWalletFile Nothing
  pure $ either (const Nothing) id $ mres

-- | Try to write `.last-wallet` file to set name of wallet
setLastStorage :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => Maybe WalletName -> m ()
setLastStorage mnane = do
  logWrite $ "Writing last storage file " <> lastWalletFile
  storeValue lastWalletFile mnane
