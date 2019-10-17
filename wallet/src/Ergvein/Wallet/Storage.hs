module Ergvein.Wallet.Storage(
    initWallet
  , readWalletFile
  ) where

import Data.Sequence
import Ergvein.Aeson
import Ergvein.IO
import Ergvein.Crypto.Constants
import Ergvein.Crypto.Keys
import System.FilePath
import System.Directory

data WalletData = WalletData
  { mnemonic :: Mnemonic
  , privateKeys :: Seq Base58
  }
  deriving (Show)
$(deriveJSON defaultOptions ''WalletData)

getWalletDirectory :: IO FilePath
getWalletDirectory = do
  userHomeDirectory <- getHomeDirectory
  return $ userHomeDirectory </> ".ergvein"

walletFileName :: String
walletFileName = "wallet.json"

-- | Creates wallet file with mnemonic phrase
initWallet :: Mnemonic -> IO ()
initWallet mnemonic = saveWalletFile walletData
  where
    walletData = WalletData {mnemonic = mnemonic, privateKeys = empty}

addXPrvKey :: WalletData -> Base58 -> WalletData
addXPrvKey (WalletData {mnemonic = mnemonic, privateKeys = privateKeys}) xPrvKey =
  WalletData {mnemonic = mnemonic, privateKeys = privateKeys |> xPrvKey}

readWalletFile :: IO (Maybe WalletData)
readWalletFile = do
  walletDirectory <- getWalletDirectory
  readJson $ walletDirectory </> walletFileName

saveWalletFile :: WalletData -> IO ()
saveWalletFile walletData = do
  walletDirectory <- getWalletDirectory
  createDirectoryIfMissing False walletDirectory
  writeJson (walletDirectory </> walletFileName) walletData
