module Ergvein.Wallet.Localization.AuthInfo
  (
    AuthInfoAlert(..)
  ) where

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Storage

data AuthInfoAlert = CreateStorageAlert !StorageAlert | GenerateECIESKeyAlert | LoadStorageAlert !StorageAlert
    deriving Eq

instance LocalizedPrint AuthInfoAlert where
  localizedShow l v = case l of
    English -> case v of
      CreateStorageAlert e -> "Failed to create storage: " <> localizedShow l e
      GenerateECIESKeyAlert -> "Failed to generate an ECIES secret key from password"
      LoadStorageAlert e -> "Failed to load wallet: " <> localizedShow l e
    Russian -> case v of
      CreateStorageAlert e -> "Не удалось создать хранилище: " <> localizedShow l e
      GenerateECIESKeyAlert -> "Не удалось сгенерировать ключ ECIES шифрования из пароля"
      LoadStorageAlert e -> "Ошибка загрузки кошелька: " <> localizedShow l e
