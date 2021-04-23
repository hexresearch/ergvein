module Ergvein.Wallet.Localize.Storage
  (
    StorageAlert(..)
  ) where

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize.Native
import Ergvein.Core.Store

import Data.Text

instance LocalizedPrint StorageAlert where
  localizedShow l v = case l of
    English -> case v of
      SADecodeError e -> "Storage loading error: " <> e
      SALoadedSucc    -> "Storage loaded"
      SANativeAlert a -> localizedShow l a
      SAMnemonicFail t -> "Failed to produce seed from mnemonic: " <> t
      SACryptoError e -> "Cryptographic error: " <> e
      SADecryptError _ -> "Wrong password. Failed to decrypt"
    Russian -> case v of
      SADecodeError e -> "Ошибка загрузки хранилища: " <> e
      SALoadedSucc    -> "Хранилище загружено"
      SANativeAlert a -> localizedShow l a
      SAMnemonicFail t -> "Не удалось создать сид из мнемоники: " <> t
      SACryptoError e -> "Ошибка криптографии: " <> e
      SADecryptError _ -> "Неправильный пароль. Не удалось расшифровать"
