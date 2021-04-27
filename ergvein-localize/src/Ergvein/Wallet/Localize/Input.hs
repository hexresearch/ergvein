{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Localize.Input
  (
    InputStrings(..)
  ) where

import Ergvein.Wallet.Language
import Sepulcas.Elements.Input.Class

instance LocalizedPrint InputStrings where
  localizedShow l v = case l of
    English -> case v of
      IntParseError          -> "Failed to parse integer"
      WordParseError         -> "Failed to parse natural number"
    Russian -> case v of
      IntParseError          -> "Не удалось прочитать целое число"
      WordParseError         -> "Число должно быть целочисленным или 0"
