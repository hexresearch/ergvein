module Ergvein.Wallet.Storage.Util(
    addXPrvKeyToKeystore
  , addXPubKeyToKeystore
  , getLastSeenHeight
  , encryptPrvStorage
  , decryptPrvStorage
  , passwordToECIESPrvKey
  , createPrvStorage
  , createPubKeystore
  , createStorage
  , storageFilePrefix
  , saveStorageToFile
  , saveStorageSafelyToFile
  , loadStorageFromFile
  , listStorages
  , getLastStorage
  , setLastStorage
  , generateMissingPrvKeys
  , generateMissingPrvKeysHelper
  ) where

import Control.Lens
import Control.Monad
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
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Localization.Native
import Ergvein.Wallet.Localization.Storage
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys

import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as MI
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V

addXPrvKeyToKeystore :: KeyPurpose -> EgvXPrvKey -> PrvKeystore -> PrvKeystore
addXPrvKeyToKeystore External key (PrvKeystore master external internal) =
   PrvKeystore master (V.snoc external key) internal
addXPrvKeyToKeystore Internal key (PrvKeystore master external internal) =
   PrvKeystore master external (V.snoc internal key)

createPrvKeystore :: EgvXPrvKey -> PrvKeystore
createPrvKeystore masterPrvKey =
  let externalGen i = Just (derivePrvKey masterPrvKey External (fromIntegral i), i + 1)
      internalGen i = Just (derivePrvKey masterPrvKey Internal (fromIntegral i), i + 1)
      externalKeys  = V.unfoldrN initialExternalAddressCount externalGen 0
      internalKeys  = V.unfoldrN initialInternalAddressCount internalGen 0
  in PrvKeystore masterPrvKey externalKeys internalKeys

createPrvStorage :: Seed -> EgvRootXPrvKey -> PrvStorage
createPrvStorage seed rootPrvKey = PrvStorage seed rootPrvKey prvStorages
  where prvStorages = M.fromList [
            (currency, CurrencyPrvStorage $ createPrvKeystore $ deriveCurrencyMasterPrvKey rootPrvKey currency) |
            currency <- allCurrencies
          ]

addXPubKeyToKeystore :: KeyPurpose -> EgvXPubKey -> PubKeystore -> PubKeystore
addXPubKeyToKeystore External key (PubKeystore master external internal) =
  PubKeystore master (V.snoc external (EgvPubKeyBox key S.empty False)) internal
addXPubKeyToKeystore Internal key (PubKeystore master external internal) =
  PubKeystore master external (V.snoc internal (EgvPubKeyBox key S.empty False))

getLastSeenHeight :: Currency -> PubStorage -> Maybe BlockHeight
getLastSeenHeight cur bs = join . (fmap _currencyPubStorage'height) $ bs ^. pubStorage'currencyPubStorages . at cur

createPubKeystore :: EgvXPubKey -> PubKeystore
createPubKeystore masterPubKey =
  let keygen kp i = Just (EgvPubKeyBox (derivePubKey masterPubKey kp (fromIntegral i)) S.empty False, i + 1)
      externalKeys = V.unfoldrN initialExternalAddressCount (keygen External) 0
      internalKeys = V.unfoldrN initialInternalAddressCount (keygen Internal) 0
  in PubKeystore masterPubKey externalKeys internalKeys

createPubStorage :: Bool -> EgvRootXPrvKey -> [Currency] -> PubStorage
createPubStorage isRestored rootPrvKey cs = PubStorage rootPubKey pubStorages cs isRestored
  where restState = if isRestored then (Just 0, Just 0) else (Nothing, Nothing)
        rootPubKey = EgvRootXPubKey $ deriveXPubKey $ unEgvRootXPrvKey rootPrvKey
        mkStore c = CurrencyPubStorage {
            _currencyPubStorage'pubKeystore   = (createPubKeystore $ deriveCurrencyMasterPubKey rootPrvKey c)
          , _currencyPubStorage'transactions  = M.empty
          , _currencyPubStorage'height        = Nothing
          , _currencyPubStorage'scannedKey    = restState
          , _currencyPubStorage'utxos         = M.empty
          , _currencyPubStorage'scannedHeight = Nothing
          , _currencyPubStorage'headers       = M.empty
          , _currencyPubStorage'outgoing      = S.empty
          , _currencyPubStorage'headerSeq     = btcCheckpoints
          }
        pubStorages = M.fromList [(currency, mkStore currency) | currency <- cs]

createStorage :: MonadIO m => Bool -> Mnemonic -> (WalletName, Password) -> [Currency] -> m (Either StorageAlert WalletStorage)
createStorage isRestored mnemonic (login, pass) cs = case mnemonicToSeed "" mnemonic of
  Left err -> pure $ Left $ SAMnemonicFail $ showt err
  Right seed -> do
    let rootPrvKey = EgvRootXPrvKey $ makeXPrvKey seed
        prvStorage = createPrvStorage seed rootPrvKey
        pubStorage = createPubStorage isRestored rootPrvKey cs
    encryptPrvStorageResult <- encryptPrvStorage prvStorage pass
    case encryptPrvStorageResult of
      Left err -> pure $ Left err
      Right encryptedPrvStorage -> pure $ Right $ WalletStorage encryptedPrvStorage pubStorage login

encryptPrvStorage :: MonadIO m => PrvStorage -> Password -> m (Either StorageAlert EncryptedPrvStorage)
encryptPrvStorage prvStorage password = liftIO $ do
  salt :: ByteString <- genRandomSalt
  let secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
  iv <- genRandomIV (undefined :: AES256)
  case iv of
    Nothing -> pure $ Left $ SACryptoError "Failed to generate an AES initialization vector"
    Just iv' -> do
      let prvStorageBS = encodeUtf8 $ encodeJson prvStorage
      case encrypt secretKey iv' prvStorageBS of
        Left err -> pure $ Left $ SACryptoError $ showt err
        Right ciphertext -> pure $ Right $ EncryptedPrvStorage ciphertext salt iv'

decryptPrvStorage :: EncryptedPrvStorage -> Password -> Either StorageAlert PrvStorage
decryptPrvStorage encryptedPrvStorage password =
  case decrypt secretKey iv ciphertext of
    Left err -> Left $ SACryptoError $ showt err
    Right decryptedPrvStorage -> do
      let decodedPrvStorage = decodeJson $ decodeUtf8With lenientDecode decryptedPrvStorage
      case decodedPrvStorage of
        Left err -> Left $ SACryptoError $ showt err
        Right dps -> Right dps
  where
    salt = _encryptedPrvStorage'salt encryptedPrvStorage
    secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
    iv = _encryptedPrvStorage'iv encryptedPrvStorage
    ciphertext = _encryptedPrvStorage'ciphertext encryptedPrvStorage

encryptStorage :: (MonadIO m, MonadRandom m) => WalletStorage -> ECIESPubKey -> m (Either StorageAlert EncryptedWalletStorage)
encryptStorage storage pubKey = do
  let curve = Proxy :: Proxy Curve_X25519
  deriveEncryptResult <- deriveEncrypt curve pubKey
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
            Right (authTag, ciphertext) -> pure $ Right $ EncryptedWalletStorage ciphertext salt iv eciesPoint authTag

decryptStorage :: EncryptedWalletStorage -> ECIESPrvKey -> Either StorageAlert WalletStorage
decryptStorage encryptedStorage prvKey = do
  let curve = Proxy :: Proxy Curve_X25519
      ciphertext = _encryptedStorage'ciphertext encryptedStorage
      salt       = _encryptedStorage'salt       encryptedStorage
      iv         = _encryptedStorage'iv         encryptedStorage
      eciesPoint = _encryptedStorage'eciesPoint encryptedStorage
      authTag    = _encryptedStorage'authTag    encryptedStorage
  case deriveDecrypt curve eciesPoint prvKey of
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
  => Text -> ECIESPubKey -> WalletStorage -> m ()
saveStorageToFile caller pubKey storage = do
  let fname = storageFilePrefix <> T.replace " " "_" (_storage'walletName storage)
  logWrite $ "[" <> caller <> "]: Storing to " <> fname
  encryptedStorage <- encryptStorage storage pubKey
  case encryptedStorage of
    Left _ -> fail "Failed to encrypt storage"
    Right encStorage -> storeValue fname encStorage

-- | The same as saveStorageToFile, but does not fail and returns the error instead
saveStorageSafelyToFile :: (MonadIO m, MonadRandom m, HasStoreDir m, PlatformNatives)
  => Text -> ECIESPubKey -> WalletStorage -> m (Either StorageAlert ())
saveStorageSafelyToFile caller pubKey storage = do
  let fname = storageFilePrefix <> T.replace " " "_" (_storage'walletName storage)
  logWrite $ "[" <> caller <> "]: Storing to " <> fname
  encryptedStorage <- encryptStorage storage pubKey
  case encryptedStorage of
    Left err -> pure $ Left err
    Right encStorage -> fmap Right $ storeValue fname encStorage

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => WalletName -> Password -> m (Either StorageAlert WalletStorage)
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

-- | Generates new private keys until their number is equal to the number of public keys.
generateMissingPrvKeys :: MonadIO m => (AuthInfo, Password) -> m (Either StorageAlert AuthInfo)
generateMissingPrvKeys (authInfo, pass) = do
  let encryptedPrvStorage = view (authInfo'storage . storage'encryptedPrvStorage) authInfo
  case decryptPrvStorage encryptedPrvStorage pass of
    Left err -> pure $ Left err
    Right decryptedPrvStorage -> do
      let updatedPrvStorage = set prvStorage'currencyPrvStorages updatedPrvKeystore decryptedPrvStorage
      encryptPrvStorageResult <- encryptPrvStorage updatedPrvStorage pass
      case encryptPrvStorageResult of
        Left err -> pure $ Left err
        Right encryptedUpdatedPrvStorage -> pure $ Right $ set (authInfo'storage . storage'encryptedPrvStorage) encryptedUpdatedPrvStorage authInfo
      where
        currencyPrvStorages = view prvStorage'currencyPrvStorages decryptedPrvStorage
        currencyPubStorages = view (authInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages) authInfo
        pubKeysNumber = M.map (\currencyPubStorage -> (
            V.length $ pubKeystore'external (view currencyPubStorage'pubKeystore currencyPubStorage),
            V.length $ pubKeystore'internal (view currencyPubStorage'pubKeystore currencyPubStorage)
          )) currencyPubStorages
        updatedPrvKeystore =
          MM.merge
          MM.dropMissing
          MM.dropMissing
          (MM.zipWithMatched generateMissingPrvKeysHelper)
          currencyPrvStorages
          pubKeysNumber

generateMissingPrvKeysHelper ::
  Currency
  -> CurrencyPrvStorage -- ^ Private keystore
  -> (Int, Int)         -- ^ Total number of external and internal private keys respectively that should be stored in keystore
  -> CurrencyPrvStorage -- ^ Updated private keystore
generateMissingPrvKeysHelper _ (CurrencyPrvStorage prvKeystore) (goalExternalKeysNum, goalInternalKeysNum) =
  CurrencyPrvStorage $ PrvKeystore masterPrvKey updatedExternalPrvKeys updatedInternalPrvKeys
  where
    currentExternalKeys = prvKeystore'external prvKeystore
    currentInternalKeys = prvKeystore'internal prvKeystore
    masterPrvKey = prvKeystore'master prvKeystore
    extLength = V.length currentExternalKeys
    intLength = V.length currentInternalKeys
    updatedExternalPrvKeys = if extLength >= goalExternalKeysNum
      then currentExternalKeys
      else let
        l = goalExternalKeysNum - extLength
        v = V.unfoldrN l (\i -> Just (derivePrvKey masterPrvKey External (fromIntegral i), i+1)) extLength
        in currentExternalKeys V.++ v
    updatedInternalPrvKeys = if intLength >= goalInternalKeysNum
      then currentInternalKeys
      else let
        l = goalInternalKeysNum - intLength
        v = V.unfoldrN l (\i -> Just (derivePrvKey masterPrvKey Internal (fromIntegral i), i+1)) intLength
        in currentInternalKeys V.++ v
