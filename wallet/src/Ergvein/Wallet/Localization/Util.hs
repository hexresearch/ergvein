module Ergvein.Wallet.Localization.Util
  (
    CommonStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text


data CommonStrings =
    CSCopied
  | CSUpdated
  deriving (Eq)

instance LocalizedPrint CommonStrings where
  localizedShow l v = case l of
    English -> case v of
      CSCopied          -> "Copied"
      CSUpdated         -> "Updated"
    Russian -> case v of
      CSCopied          -> "Скопировано"
      CSUpdated         -> "Обновлено"
