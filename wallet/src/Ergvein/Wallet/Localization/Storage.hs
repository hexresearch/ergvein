module Ergvein.Wallet.Localization.Storage
  (
    StorageAlert(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Native

import Data.Text

-- Alerts regarding secure storage system
data StorageAlert
  = SADecodeError Text
  | SALoadedSucc
  | SANativeAlert NativeAlerts
  | SAMnemonicFail Text
  | SACryptoError Text
  deriving (Eq)

instance LocalizedPrint StorageAlert where
  localizedShow l v = case l of
    English -> case v of
      SADecodeError e -> "Storage loading error: " <> e
      SALoadedSucc    -> "Storage loaded"
      SANativeAlert a -> localizedShow l a
      SAMnemonicFail t -> "Failed to produce seed from mnemonic: " <> t
      SACryptoError e -> "Cryptographic error: " <> e
    Russian -> localizedShow English v
