module Ergvein.Wallet.Storage.WalletInfo (
    initWalletInfo
  , loadWalletInfo
  ) where

import Control.Monad.Except

import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Restore
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.WalletInfo
import Ergvein.Wallet.Monad
import Sepulcas.Native
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Storage.Util

import qualified Data.Text as T

initWalletInfo :: (MonadIO m, PlatformNatives, HasStoreDir m)
  => WalletSource
  -> Maybe DerivPrefix
  -> Mnemonic
  -> [Currency]
  -> WalletName
  -> Password
  -> BlockHeight
  -> Bool
  -> m (Either WalletInfoAlert WalletInfo)
initWalletInfo wt mpath mnemonic curs login pass startingHeight isPass = do
  let fname = "meta_wallet_" <> (T.replace " " "_" login)
  when (isAndroid && isPass) $ storeValue fname True True
  mstorage <- createStorage (wt == WalletRestored) mpath mnemonic (login, pass) startingHeight curs
  case mstorage of
    Left err -> do
      logWrite $ localizedShow English err
      pure $ Left $ CreateStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left _ -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right WalletInfo {
          _walletInfo'storage = s
        , _walletInfo'eciesPubKey = toPublic k
        , _walletInfo'login = login
        , _walletInfo'isUpdate = False
        , _walletInfo'isPlain = pass == ""
        }

loadWalletInfo :: (MonadIO m, HasStoreDir m, PlatformNatives)
  => WalletName
  -> Password
  -> m (Either WalletInfoAlert (WalletInfo, Password))
loadWalletInfo login pass = do
  mstorage <- loadStorageFromFile login pass
  case mstorage of
    Left err -> pure $ Left $ LoadStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left _ -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right (
          WalletInfo {
            _walletInfo'storage = s
          , _walletInfo'eciesPubKey = toPublic k
          , _walletInfo'login = login
          , _walletInfo'isUpdate = False
          , _walletInfo'isPlain = pass == ""
          }
        , pass
        )
