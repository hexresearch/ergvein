module Ergvein.Wallet.Storage.AuthInfo (
    initAuthInfo
  , loadAuthInfo
  ) where

import Control.Monad.Except
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Input
import Ergvein.Wallet.Localization.AuthInfo
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Util

initAuthInfo :: MonadIO m => Mnemonic -> [Currency] -> WalletName -> Password -> m (Either AuthInfoAlert AuthInfo)
initAuthInfo mnemonic curs login pass = do
  mstorage <- createStorage mnemonic (login, pass) curs
  case mstorage of
    Left err -> pure $ Left $ CreateStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left err -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right AuthInfo {
          _authInfo'storage = s
        , _authInfo'eciesPubKey = toPublic k
        , _authInfo'login = login
        , _authInfo'isUpdate = False
        }

loadAuthInfo :: (MonadIO m, HasStoreDir m, PlatformNatives) => WalletName -> Password -> m (Either AuthInfoAlert (AuthInfo, Password))
loadAuthInfo login pass = do
  mstorage <- loadStorageFromFile login pass
  case mstorage of
    Left err -> pure $ Left $ LoadStorageAlert err
    Right s -> case passwordToECIESPrvKey pass of
      Left err -> pure $ Left GenerateECIESKeyAlert
      Right k -> pure $ Right (
          AuthInfo {
            _authInfo'storage = s
          , _authInfo'eciesPubKey = toPublic k
          , _authInfo'login = login
          , _authInfo'isUpdate = False
          }
        , pass
        )
