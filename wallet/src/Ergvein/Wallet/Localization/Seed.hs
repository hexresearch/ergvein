module Ergvein.Wallet.Localization.Seed
  (
    SeedPageStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data SeedPageStrings =
    SPSTitle
  | SPSWarn
  | SPSWrote
  | SPSVerifyTitle
  | SPSSelectWord Int
  | SPSRestoreFromMnemonic
  | SPSRestoreFromSeed
  | SPSEnterSeed
  | SPSScanSeed
  | SPSSeedDecodeError
  | SPSReset
  | SPSEnterWord Int
  | SPSWaiting
  | SPSInvalidWord

numSuffix :: Int -> Text
numSuffix n = case (n `mod` 10) of
  1 -> "st"
  2 -> "nd"
  3 -> "rd"
  _ -> "th"

instance LocalizedPrint SeedPageStrings where
  localizedShow l v = case l of
    English -> case v of
      SPSTitle               -> "These words are your mnemonic phrase"
      SPSWarn                -> "It is the ONLY way to restore access to your wallet. Write it down or you will lost your money forever."
      SPSWrote               -> "I wrote them"
      SPSVerifyTitle         -> "Double check the mnemonic phrase"
      SPSSelectWord n        -> "Select the " <> showt n <> numSuffix n <> " word"
      SPSRestoreFromMnemonic -> "Restoring wallet from mnemonic phrase"
      SPSRestoreFromSeed     -> "Restoring wallet from seed"
      SPSEnterSeed           -> "Enter seed"
      SPSScanSeed            -> "Scan QR code"
      SPSSeedDecodeError     -> "Failed to decode the seed"
      SPSReset               -> "Reset and start again"
      SPSEnterWord n         -> "Enter the " <> showt n <> numSuffix n <> " word"
      SPSWaiting             -> "Waiting for input..."
      SPSInvalidWord         -> "Invalid word"
    Russian -> case v of
      SPSTitle               -> "Слова мнемонической фразы от вашего кошелька"
      SPSWarn                -> "Эта мнемоническая фраза — единственный способ восстановить ваш кошелёк. Запишите их, иначе вы можете потерять свои деньги. Навсегда."
      SPSWrote               -> "Я записал мнемоническую фразу"
      SPSVerifyTitle         -> "Перепроверьте мнемоническую фразу"
      SPSSelectWord n        -> "Выберите " <> showt n <> "-е слово"
      SPSRestoreFromMnemonic -> "Восстановление кошелька из мнемонической фразы"
      SPSRestoreFromSeed     -> "Восстановление кошелька из seed"
      SPSEnterSeed           -> "Введите seed"
      SPSScanSeed            -> "Cканировать QR-код"
      SPSSeedDecodeError     -> "Не удалось декодировать seed"
      SPSReset               -> "Сбросить и начать заново"
      SPSEnterWord n         -> "Введите " <> showt n <> "-е слово"
      SPSWaiting             -> "Ожидаю ввода..."
      SPSInvalidWord         -> "Недопустимое слово"
