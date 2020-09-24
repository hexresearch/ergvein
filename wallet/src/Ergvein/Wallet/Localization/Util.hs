module Ergvein.Wallet.Localization.Util
  (
    CommonStrings(..)
  ) where

import Ergvein.Wallet.Language

data CommonStrings =
    CSCopy
  | CSCopied
  | CSPaste
  | CSUpdated
  | CSOn
  | CSOff
  | CSForward
  | CSShare
  | CSShareQR
  | CSScanQR
  deriving (Eq)

instance LocalizedPrint CommonStrings where
  localizedShow l v = case l of
    English -> case v of
      CSCopy            -> "Copy"
      CSCopied          -> "Copied"
      CSPaste           -> "Paste"
      CSUpdated         -> "Updated"
      CSOn              -> "On"
      CSOff             -> "Off"
      CSForward         -> "Forward"
      CSShare           -> "Share"
      CSShareQR         -> "Share QR code"
      CSScanQR          -> "Scan QR code"

    Russian -> case v of
      CSCopy            -> "Скопировать"
      CSCopied          -> "Скопировано"
      CSPaste           -> "Вставить"
      CSUpdated         -> "Обновлено"
      CSOn              -> "Вкл"
      CSOff             -> "Выкл"
      CSForward         -> "Далее"
      CSShare           -> "Поделиться"
      CSShareQR         -> "Поделиться QR-кодом"
      CSScanQR          -> "Сканировать QR-код"
