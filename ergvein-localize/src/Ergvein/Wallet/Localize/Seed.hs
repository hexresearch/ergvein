module Ergvein.Wallet.Localize.Seed
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
  | SPSVerifyDescr
  | SPSVerifyError
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
  | SPSBackupTitle
  | SPSBackupText1
  | SPSBackupText2
  | SPSBackupNow
  | SPSBackupLater

numSuffix :: Int -> Text
numSuffix n = case n `mod` 10 of
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
      SPSVerifyTitle          -> "Verify mnemonic phrase"
      SPSVerifyDescr          -> "Tap the words to put them next to each other in the correct order."
      SPSVerifyError          -> "Invalid order. Try again!"
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
      SPSBase58               -> "Encrypted mnemonic"
      SPSPlainTitle i         -> "Enter the mnemonic phrase of length " <> showt i
      SPSBase58Title          -> "Enter the encrypted mnemonic"
      SPSBackupTitle          -> "Back up your mnemonic phrase"
      SPSBackupText1          -> "Your secret 12-words recovery phrase is the only way to recover your funds if you lose access to your wallet."
      SPSBackupText2          -> "Write it down safely and store it in a secure location."
      SPSBackupNow            -> "Backup now"
      SPSBackupLater          -> "Do it later"
    Russian -> case v of
      SPSTitle                -> "Слова мнемонической фразы от вашего кошелька"
      SPSWarn                 -> "Эта мнемоническая фраза — единственный способ восстановить ваш кошелёк. Запишите их, иначе вы можете потерять свои деньги. Навсегда."
      SPSWrote                -> "Я записал мнемоническую фразу"
      SPSVerifyTitle          -> "Перепроверьте мнемоническую фразу"
      SPSVerifyDescr          -> "Нажмите на слова, чтобы поместить их рядом друг с другом в правильном порядке."
      SPSVerifyError          -> "Неверный порядок. Попробуйте ещё раз!"
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
      SPSBase58               -> "Зашифрованная мнемоника"
      SPSPlainTitle i         -> "Введите мнемоническую фразу длиной " <> showt i
      SPSBase58Title          -> "Введите зашифрованную мнемоническую фразу"
      SPSBackupTitle          -> "Сделайте резервную копию вашей мнемонической фразы"
      SPSBackupText1          -> "Ваша мнемоническая фраза из 12 слов - это единственный способ восстановить ваши средства, если вы потеряли доступ к своему кошельку."
      SPSBackupText2          -> "Запишите ёё и сохраните в надёжном месте."
      SPSBackupNow            -> "Сделать резервную копию сейчас"
      SPSBackupLater          -> "Сделать это позже"
