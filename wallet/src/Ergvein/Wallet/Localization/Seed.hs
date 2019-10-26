module Ergvein.Wallet.Localization.Seed
  (
    SeedPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language
import Reflex.Localize
import Data.Text

data SeedPageStrings =
    SPSTitle
  | SPSWarn
  | SPSWrote
  | SPSVerifyTitle
  | SPSSelectWord Int
  | SPSRestoreTitle
  | SPSReset
  | SPSEnterWord Int
  | SPSWaiting

numSuffix :: Int -> Text
numSuffix n = case (n `mod` 10) of
  1 -> "st"
  2 -> "nd"
  3 -> "rd"
  _ -> "th"

instance LocalizedPrint SeedPageStrings where
  localizedShow l v = case l of
    English -> case v of
      SPSTitle        -> "These words are your seed phrase"
      SPSWarn         -> "It is the ONLY way to restore access to your wallet. Write it down or you will lost your money forever."
      SPSWrote        -> "I wrote them"
      SPSVerifyTitle  -> "Double check the seed phrase"
      SPSSelectWord n -> "Select the " <> showt n <> numSuffix n <> " word"
      SPSRestoreTitle -> "Seed restore"
      SPSReset        -> "Reset and start again"
      SPSEnterWord n  -> "Enter the " <> showt n <> numSuffix n <> " word"
      SPSWaiting      -> "Waiting for input..."
    Russian -> case v of
      SPSTitle        -> "Слова мнемоники от вашего кошелька"
      SPSWarn         -> "Эта мнемоника — единственный способ восстановить ваш кошелёк. Запишите их, иначе вы можете потерять свои деньги. Навсегда."
      SPSWrote        -> "Я записал мнемонику"
      SPSVerifyTitle  -> "Перепроверьте мнемонику"
      SPSSelectWord n -> "Выберите " <> showt n <> "-е слово"
      SPSRestoreTitle -> "Восстановление кошелька"
      SPSReset        -> "Сбросить и начать заново"
      SPSEnterWord n  -> "Введите " <> showt n <> "-е слово"
      SPSWaiting      -> "Ожидаю ввода..."
