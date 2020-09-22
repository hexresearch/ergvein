module Ergvein.Wallet.Localization.Initial
  (
    InitialPageStrings(..)
  ) where

import Ergvein.Wallet.Language

data InitialPageStrings =
    IPSCreate
  | IPSRestore
  | IPSNetwork
  | IPSSelectWallet
  | IPSOtherOptions
  | IPSPinCode
  | IPSPinCodeError
  | IPSLastWallet

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate       -> "Create wallet"
      IPSRestore      -> "Restore wallet"
      IPSNetwork      -> "Setup indexers"
      IPSSelectWallet -> "Select wallet"
      IPSOtherOptions -> "Either"
      IPSPinCode      -> "PIN code:"
      IPSPinCodeError -> "Invalid code"
      IPSLastWallet   -> "Last used wallet"
    Russian -> case v of
      IPSCreate       -> "Создать кошелёк"
      IPSRestore      -> "Восстановить кошелёк"
      IPSNetwork      -> "Настроить индексеры"
      IPSSelectWallet -> "Выберите кошелёк"
      IPSOtherOptions -> "Или"
      IPSPinCode      -> "ПИН код:"
      IPSPinCodeError -> "Неправильный код"
      IPSLastWallet   -> "Последний использованный кошелек"
