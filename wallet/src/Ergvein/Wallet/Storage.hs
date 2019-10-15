module Ergvein.Wallet.Storage(
    initWallet
  -- , createWalletFile
  ) where

import Ergvein.Aeson
import Ergvein.IO
import Ergvein.Crypto.Constants
import Ergvein.Crypto.Keys
import System.FilePath
import System.Directory

data WalletData = WalletData
  { mnemonic :: Mnemonic
  , privateKeys :: [String]
  }
$(deriveJSON defaultOptions ''WalletData)

getWalletDirectory :: IO FilePath
getWalletDirectory = do
  userHomeDirectory <- getHomeDirectory
  return $ userHomeDirectory </> ".ergvein"

walletFileName :: String
walletFileName = "wallet.json"

initWallet :: Mnemonic -> IO ()
initWallet mnemonic = do
  walletDirectory <- getWalletDirectory
  createDirectoryIfMissing False walletDirectory
  writeJson (walletDirectory </> walletFileName) walletData
  where
    walletData = WalletData {mnemonic = mnemonic, privateKeys = []}
