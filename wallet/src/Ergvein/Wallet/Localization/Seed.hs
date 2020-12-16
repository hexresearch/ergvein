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
  | SPSScanQR
  | SPSMnemonicDecodeError
  | SPSReset
  | SPSEnterWord Int
  | SPSWaiting
  | SPSInvalidWord
  | SPSDone
  | SPSExtraWords
  | SPSMisspelled
  | SPSMisspelledWord (Int, Text)
  | SPSTypeTitle
  | SPSLengthTitle
  | SPSPlain
  | SPSBase58
  | SPSPlainTitle Int
  | SPSBase58Title

numSuffix :: Int -> Text
numSuffix n = case (n `mod` 10) of
  1 -> "st"
  2 -> "nd"
  3 -> "rd"
  _ -> "th"

instance LocalizedPrint SeedPageStrings where
  localizedShow l v = case l of
    English -> case v of
      SPSTitle                -> "These words are your mnemonic phrase"
      SPSWarn                 -> "It is the ONLY way to restore access to your wallet. Write it down or you will lost your money forever."
      SPSWrote                -> "I wrote them"
      SPSVerifyTitle          -> "Double check the mnemonic phrase"
      SPSSelectWord n         -> "Select the " <> showt n <> numSuffix n <> " word"
      SPSScanQR               -> "Scan QR code"
      SPSMnemonicDecodeError  -> "Failed to decode mnemonic"
      SPSReset                -> "Reset and start again"
      SPSEnterWord n          -> "Enter the " <> showt n <> numSuffix n <> " word"
      SPSWaiting              -> "Waiting for input..."
      SPSInvalidWord          -> "Invalid word"
      SPSDone                 -> "Done!"
      SPSExtraWords           -> "Error! You have extra words!"
      SPSMisspelled           -> "Some words are misspelled"
      SPSMisspelledWord (i,w) -> "#" <> showt i <> ": " <> w
      SPSTypeTitle            -> "Select mnemonic type"
      SPSLengthTitle          -> "Select mnemonic's length"
      SPSPlain                -> "Plain text"
      SPSBase58               -> "Base58"
      SPSPlainTitle i         -> "Enter the mnemonic phrase of length " <> showt i
      SPSBase58Title          -> "Enter base58-encoded mnemonic"
    Russian -> case v of
      SPSTitle                -> "Слова мнемонической фразы от вашего кошелька"
      SPSWarn                 -> "Эта мнемоническая фраза — единственный способ восстановить ваш кошелёк. Запишите их, иначе вы можете потерять свои деньги. Навсегда."
      SPSWrote                -> "Я записал мнемоническую фразу"
      SPSVerifyTitle          -> "Перепроверьте мнемоническую фразу"
      SPSSelectWord n         -> "Выберите " <> showt n <> "-е слово"
      SPSScanQR               -> "Cканировать QR-код"
      SPSMnemonicDecodeError  -> "Не удалось декодировать мнемоническую фразу"
      SPSReset                -> "Сбросить и начать заново"
      SPSEnterWord n          -> "Введите " <> showt n <> "-е слово"
      SPSWaiting              -> "Ожидаю ввода..."
      SPSInvalidWord          -> "Недопустимое слово"
      SPSDone                 -> "Готово!"
      SPSExtraWords           -> "Ошибка! Вы ввели лишние слова!"
      SPSMisspelled           -> "Некоторые слова с ошибкой, не словарные"
      SPSMisspelledWord (i,w) -> "№" <> showt i <> ": " <> w
      SPSTypeTitle            -> "Выберите тип мнемоники"
      SPSLengthTitle          -> "Выберите длину мнемоники"
      SPSPlain                -> "Словарный"
      SPSBase58               -> "Base58"
      SPSPlainTitle i         -> "Введите мнемоническую фразу длиной " <> showt i
      SPSBase58Title          -> "Введите мнемоническую фразу в кодировке base58"
