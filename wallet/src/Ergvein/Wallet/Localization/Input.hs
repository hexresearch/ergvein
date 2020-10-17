module Ergvein.Wallet.Localization.Input
  (
    InputStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data InputStrings =
    IntParseError
  | WordParseError
  deriving (Eq, Show)

instance LocalizedPrint InputStrings where
  localizedShow l v = case l of
    English -> case v of
      IntParseError          -> "Failed to parse integer"
      WordParseError         -> "Failed to parse natural number"
    Russian -> case v of
      IntParseError          -> "Не удалось прочитать целое число"
      WordParseError         -> "Число должно быть целочисленным или 0"
