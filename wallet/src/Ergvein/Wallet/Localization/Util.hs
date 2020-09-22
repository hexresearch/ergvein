module Ergvein.Wallet.Localization.Util
  (
    CommonStrings(..)
  ) where

import Ergvein.Wallet.Language

data CommonStrings =
    CSCopied
  | CSUpdated
  | CSOn
  | CSOff
  deriving (Eq)

instance LocalizedPrint CommonStrings where
  localizedShow l v = case l of
    English -> case v of
      CSCopied          -> "Copied"
      CSUpdated         -> "Updated"
      CSOn              -> "On"
      CSOff             -> "Off"
    Russian -> case v of
      CSCopied          -> "Скопировано"
      CSUpdated         -> "Обновлено"
      CSOn              -> "Вкл"
      CSOff             -> "Выкл"
