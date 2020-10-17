module Ergvein.Wallet.Localization.IP
  (
    IPStrings(..)
  ) where

import Ergvein.Text
import Ergvein.Wallet.Language

import Data.Text

data IPStrings =
  IPParseFailed
  deriving (Eq, Show)

instance LocalizedPrint IPStrings where
  localizedShow l v = case l of
    English -> case v of
      IPParseFailed          -> "Failed to parse IP address"
    Russian -> case v of
      IPParseFailed          -> "IP адрес неверного формата"
