module Ergvein.Wallet.Localization.Initial
  (
    InitialPageStrings(..)
  ) where

import Ergvein.Wallet.Language

data InitialPageStrings =
    IPSCreate
  | IPSRestore
  | IPSSelectWallet
  | IPSOtherOptions
  | IPSPinCode
  | IPSPinCodeError
  | IPSLastWallet

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate       -> "Create new wallet"
      IPSRestore      -> "Restore wallet"
      IPSSelectWallet -> "Select wallet"
      IPSOtherOptions -> "Either"
      IPSPinCode      -> "PIN code:"
      IPSPinCodeError -> "Invalid code"
      IPSLastWallet   -> "Last used wallet"
    Russian -> case v of
      IPSCreate       -> "Создать новый кошелёк"
      IPSRestore      -> "Восстановить кошелёк"
      IPSSelectWallet -> "Выберите кошелёк"
      IPSOtherOptions -> "Или"
      IPSPinCode      -> "ПИН код:"
      IPSPinCodeError -> "Неправильный код"
      IPSLastWallet   -> "Последний использованный кошелек"
