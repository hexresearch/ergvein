module Ergvein.Wallet.Localization.Util
  (
    CommonStrings(..)
  ) where

import Ergvein.Wallet.Language

data CommonStrings =
    CSCopy
  | CSCopied
  | CSUpdated
  | CSOn
  | CSOff
  | CSForward
  | CSShare
  | CSShareQR
  deriving (Eq)

instance LocalizedPrint CommonStrings where
  localizedShow l v = case l of
    English -> case v of
      CSCopy            -> "Copy"
      CSCopied          -> "Copied"
      CSUpdated         -> "Updated"
      CSOn              -> "On"
      CSOff             -> "Off"
      CSForward         -> "Forward"
      CSShare           -> "Share"
      CSShareQR         -> "Share QR code"

    Russian -> case v of
      CSCopy            -> "Скопировать"
      CSCopied          -> "Скопировано"
      CSUpdated         -> "Обновлено"
      CSOn              -> "Вкл"
      CSOff             -> "Выкл"
      CSForward         -> "Далее"
      CSShare           -> "Поделиться"
      CSShareQR         -> "Поделиться QR-кодом"