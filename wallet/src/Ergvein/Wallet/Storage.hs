module Ergvein.Wallet.Storage(
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

import Ergvein.Aeson
import Ergvein.IO
import Ergvein.Crypto
import Ergvein.Wallet.Input     (Password)
import System.FilePath
import System.Directory
import Data.Text                (Text)
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.ByteString          (ByteString)
import Data.Sequence
import Data.Proxy
import Data.ByteArray           (convert)

import qualified Data.ByteString.Base64 as Base64 (encode, decodeLenient)

data WalletData = WalletData
  { mnemonic    :: Mnemonic
  , privateKeys :: Seq Base58
  } deriving (Show)
$(deriveJSON defaultOptions ''WalletData)

data EncryptedWalletData = EncryptedWalletData
  { encryptedData   :: Text
  , salt            :: Text
  , initVector      :: Text
  } deriving (Show)
$(deriveJSON defaultOptions ''EncryptedWalletData)

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
  iv' <- genRandomIV (undefined :: AES256)
  case iv' of
    Nothing -> error "Failed to generate an initialization vector"
    Just iv -> do
      let walletDataBS = encodeUtf8 $ encodeJson walletData
      let encryptedData = encrypt secretKey iv walletDataBS
      case encryptedData of
        Left err -> error $ show err
        Right eData -> return EncryptedWalletData {
            encryptedData = decodeUtf8With lenientDecode $ Base64.encode eData
          , salt = decodeUtf8With lenientDecode $ Base64.encode salt
          , initVector = decodeUtf8With lenientDecode $ Base64.encode (convert iv :: ByteString)
          }

decryptWalletData :: EncryptedWalletData -> Password -> Either String WalletData
decryptWalletData encryptedWalletData password =
  case iv' of
    Nothing -> Left "Failed to decode the initialization vector"
    Just iv -> case decrypt secretKey iv encryptedDataBS of
      Left err -> Left $ show err
      Right decryptedData -> case walletData of
        Left err -> Left $ show err
        Right wd -> Right wd
        where
          walletData = decodeJson $ decodeUtf8With lenientDecode decryptedData
  where
    saltBS = Base64.decodeLenient $ encodeUtf8 $ salt encryptedWalletData
    secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params (encodeUtf8 password) saltBS) :: Key AES256 ByteString
    iv' = makeIV $ Base64.decodeLenient $ encodeUtf8 $ initVector encryptedWalletData :: Maybe (IV AES256)
    encryptedDataBS = Base64.decodeLenient $ encodeUtf8 $ encryptedData encryptedWalletData

encryptWalletFile :: WalletData -> Point Curve_X25519 -> IO EncryptedWalletData
encryptWalletFile walletData publicKey = do
  let curve = Proxy :: Proxy Curve_X25519
  deriveEncryptResult <- deriveEncrypt curve publicKey
  case deriveEncryptResult of
    CryptoFailed err -> error $ show err
    CryptoPassed (point, sharedSecret) -> do
      salt :: ByteString <- genRandomSalt
      let secretKey = Key (fastPBKDF2_SHA256 defaultPBKDF2Params sharedSecret salt) :: Key AES256 ByteString
      iv' <- genRandomIV (undefined :: AES256)
      case iv' of
        Nothing -> error "Failed to generate an initialization vector"
        Just iv -> do
          let walletDataBS = encodeUtf8 $ encodeJson walletData
          let encryptedData = encryptWithAEAD AEAD_GCM secretKey iv ("" :: ByteString) walletDataBS 32
          case encryptedData of
            Left err -> error $ show err
            Right (tag, eData) -> return EncryptedWalletData {
                encryptedData = decodeUtf8With lenientDecode $ Base64.encode eData
              , salt = decodeUtf8With lenientDecode $ Base64.encode salt
              , initVector = decodeUtf8With lenientDecode $ Base64.encode (convert iv :: ByteString)
              }
