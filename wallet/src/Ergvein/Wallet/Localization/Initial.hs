module Ergvein.Wallet.Localization.Initial
  (
    InitialPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data InitialPageStrings =
    IPSCreate
  | IPSRestore
  | IPSSelectWallet

instance LocalizedPrint InitialPageStrings where
  localizedShow l v = case l of
    English -> case v of
      IPSCreate   -> "Create wallet"
      IPSRestore  -> "Restore wallet"
      IPSSelectWallet -> "Select wallet"
    Russian -> case v of
      IPSCreate   -> "Создать кошелёк"
      IPSRestore  -> "Восстановить кошелёк"
      IPSSelectWallet -> "Выберите кошелёк"
