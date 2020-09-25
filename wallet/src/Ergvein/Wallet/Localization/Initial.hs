module Ergvein.Wallet.Localization.Initial
  (
    InitialPageStrings(..)
  ) where

import Ergvein.Wallet.Language

data InitialPageStrings =
    IPSCreate
  | IPSRestore
  | IPSChooseRestorationMethod
  | IPSRestoreFromSeed
  | IPSNetwork
  | IPSSelectWallet
  | IPSOtherOptions
  | IPSPinCode
  | IPSPinCodeError
  | IPSLastWallet

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate                  -> "Create new wallet"
      IPSRestore                 -> "Restore wallet"
      IPSChooseRestorationMethod -> "Choose restoration method"
      IPSRestoreFromSeed         -> "Seed"
      IPSSelectWallet            -> "Select wallet"
      IPSOtherOptions            -> "Either"
      IPSPinCode                 -> "PIN code:"
      IPSPinCodeError            -> "Invalid code"
      IPSLastWallet              -> "Last used wallet"
      IPSNetwork                 -> "Setup indexers"
    Russian -> case v of
      IPSCreate                  -> "Создать новый кошелёк"
      IPSRestore                 -> "Восстановить кошелёк"
      IPSChooseRestorationMethod -> "Выберите метод восстановления"
      IPSRestoreFromSeed         -> "Seed"
      IPSSelectWallet            -> "Выберите кошелёк"
      IPSOtherOptions            -> "Или"
      IPSPinCode                 -> "ПИН код:"
      IPSPinCodeError            -> "Неправильный код"
      IPSLastWallet              -> "Последний использованный кошелек"
      IPSNetwork                 -> "Настроить индексеры"
