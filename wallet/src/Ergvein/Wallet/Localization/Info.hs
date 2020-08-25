module Ergvein.Wallet.Localization.Info(
    InfoPageStrings(..)
  ) where

import Ergvein.Types.Currency
import Ergvein.Wallet.Language

data InfoPageStrings =
    InfoTitle !Currency
  | NameWallet
  | TotalBalance
  | ConfirmedBalance
  | MasterPubKey

instance LocalizedPrint InfoPageStrings where
  localizedShow l v = case l of
    English -> case v of
      InfoTitle c       -> "Info wallet for " <> currencyName c
      NameWallet        -> "Name of wallet: "
      TotalBalance      -> "Total balance: "
      ConfirmedBalance  -> "Confirmed balance: "
      MasterPubKey      -> "Master public key: "
    Russian -> case v of
      InfoTitle c       -> "Информация о кошельке " <> currencyName c
      NameWallet        -> "Наименование кошелька: "
      TotalBalance      -> "Итоговый баланс: "
      ConfirmedBalance  -> "Подтвержденный баланс: "
      MasterPubKey      -> "Открытый мастер-ключ: "
