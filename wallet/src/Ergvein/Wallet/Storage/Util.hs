module Ergvein.Wallet.Storage.Util(
    WalletData(..)
  , EncryptedWalletData(..)
  , encryptWalletData
  , decryptWalletData
  , createWallet
  , createStorage
  , storageFilePrefix
  , loadStorageFromFile
  ) where

import Control.Monad.IO.Class
import Data.ByteArray           (convert)
import Data.ByteString          (ByteString)
import Data.ByteString.Base64   (encode, decodeLenient)
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

type Password = Text
type WalletName = Text

createWallet :: Mnemonic -> Either StorageAlerts WalletData
createWallet mnemonic = case mnemonicToSeed "" mnemonic of
  Left err -> Left $ SAMnemonicFail $ T.pack err
  Right seed -> let
    root = makeXPrvKey seed
    masters = M.fromList $ fmap (\c -> (c, deriveCurrencyKey root c)) allCurrencies
    in Right $ WalletData seed (EgvRootKey root) masters

createStorage :: MonadIO m => Mnemonic -> (WalletName, Password) -> m (Either StorageAlerts ErgveinStorage)
createStorage mnemonic (login, pass) = either (pure . Left) (\wd -> do
  ewd <- encryptWalletData wd pass
  pure $ Right $ ErgveinStorage ewd mempty login) $ createWallet mnemonic

encryptWalletData :: MonadIO m => WalletData -> Password -> m EncryptedWalletData
encryptWalletData walletData password = liftIO $ do
  salt :: ByteString <- genRandomSalt
  let secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) salt) :: Key AES256 ByteString
  mInitIV <- genRandomIV (undefined :: AES256)
  case mInitIV of
    Nothing -> error "Failed to generate an initialization vector"
    Just initIV -> do
      let walletDataBS = encodeUtf8 $ encodeJson walletData
      let encryptedData = encrypt secretKey initIV walletDataBS
      case encryptedData of
        Left err -> error $ show err
        Right eData -> return EncryptedWalletData {
            encryptedData = decodeUtf8With lenientDecode $ encode eData
          , salt = decodeUtf8With lenientDecode $ encode salt
          , initVector = decodeUtf8With lenientDecode $ encode (convert initIV :: ByteString)
          }

decryptWalletData :: EncryptedWalletData -> Password -> Either Text WalletData
decryptWalletData encryptedWalletData password =
  case initIV of
    Nothing -> Left "Failed to decode the initialization vector"
    Just iv -> case decrypt secretKey iv encryptedDataBS of
      Left err -> Left $ showt err
      Right decryptedData -> case walletData of
        Left err -> Left $ showt err
        Right wd -> Right wd
        where
          walletData = decodeJson $ decodeUtf8With lenientDecode decryptedData
  where
    saltBS = decodeLenient $ encodeUtf8 $ salt encryptedWalletData
    secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) saltBS) :: Key AES256 ByteString
    initIV = makeIV $ decodeLenient $ encodeUtf8 $ initVector encryptedWalletData :: Maybe (IV AES256)
    encryptedDataBS = decodeLenient $ encodeUtf8 $ encryptedData encryptedWalletData


storageFilePrefix :: Text
storageFilePrefix = "wallet_"

-- TODO: Actually decrypt the storage
decryptStorage :: MonadIO m => Password -> Text -> m (Either StorageAlerts ErgveinStorage)
decryptStorage pass txt = pure $ either (Left . SADecodeError) Right $ text2json txt

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => WalletName -> Password -> m (Either StorageAlerts ErgveinStorage)
loadStorageFromFile login pass = do
  let fname = storageFilePrefix <> T.replace " " "_" login
  storageResp <- readStoredFile fname
  either (pure . Left . SANativeAlert) (decryptStorage pass . T.concat) storageResp

-- Alerts regarding secure storage system
data StorageAlerts
  = SADecodeError Text
  | SALoadedSucc
  | SANativeAlert NativeAlerts
  | SAMnemonicFail Text
  deriving (Eq)

instance LocalizedPrint StorageAlerts where
  localizedShow l v = case l of
    English -> case v of
      SADecodeError e -> "Storage loading error: " <> e
      SALoadedSucc    -> "Storage loaded"
      SANativeAlert a -> localizedShow l a
      SAMnemonicFail t -> "Failed to produce seed from mnemonic: " <> t
    Russian -> localizedShow English v
