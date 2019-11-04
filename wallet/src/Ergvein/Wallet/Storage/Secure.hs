module Ergvein.Wallet.Storage.Secure(
    WalletData(..)
  , EncryptedWalletData(..)
  , createWallet
  , readWalletData
  , saveWalletData
  , encryptWalletData
  , decryptWalletData
  , addXPrvKey
  , getWalletDirectory
  , walletFileName
  ) where

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
import Ergvein.Wallet.Input     (Password)
import Ergvein.Wallet.Storage.Secure.Data
import System.Directory
import System.FilePath

-- | Get user home directory location with ".ergvein" directory appended.
getWalletDirectory :: IO FilePath
getWalletDirectory = do
  userHomeDirectory <- getHomeDirectory
  return $ userHomeDirectory </> ".ergvein"

-- | Specify wallet file name
walletFileName :: String
walletFileName = "wallet.json"

-- | Creates wallet file with mnemonic phrase
createWallet :: Mnemonic -> IO ()
createWallet mnemonic = saveWalletData walletData
  where
    walletData = WalletData {mnemonic = mnemonic, privateKeys = empty}

-- | Add extended private key to the WalletData value
addXPrvKey :: WalletData -> Base58 -> WalletData
addXPrvKey WalletData {mnemonic = mnemonic, privateKeys = privateKeys} xPrvKey =
  WalletData {mnemonic = mnemonic, privateKeys = privateKeys |> xPrvKey}

-- | Read wallet file
readWalletData :: IO (Maybe WalletData)
readWalletData = do
  walletDirectory <- getWalletDirectory
  readJson $ walletDirectory </> walletFileName

-- | Save wallet data to wallet file
saveWalletData :: WalletData -> IO ()
saveWalletData walletData = do
  walletDirectory <- getWalletDirectory
  createDirectoryIfMissing False walletDirectory
  writeJson (walletDirectory </> walletFileName) walletData

encryptWalletData :: WalletData -> Password -> IO EncryptedWalletData
encryptWalletData walletData password = do
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

decryptWalletData :: EncryptedWalletData -> Password -> Either String WalletData
decryptWalletData encryptedWalletData password =
  case initIV of
    Nothing -> Left "Failed to decode the initialization vector"
    Just iv -> case decrypt secretKey iv encryptedDataBS of
      Left err -> Left $ show err
      Right decryptedData -> case walletData of
        Left err -> Left $ show err
        Right wd -> Right wd
        where
          walletData = decodeJson $ decodeUtf8With lenientDecode decryptedData
  where
    saltBS = decodeLenient $ encodeUtf8 $ salt encryptedWalletData
    secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) saltBS) :: Key AES256 ByteString
    initIV = makeIV $ decodeLenient $ encodeUtf8 $ initVector encryptedWalletData :: Maybe (IV AES256)
    encryptedDataBS = decodeLenient $ encodeUtf8 $ encryptedData encryptedWalletData
