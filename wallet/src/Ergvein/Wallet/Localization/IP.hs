module Ergvein.Wallet.Localization.IP
  (
    IPStrings(..)
  ) where

import Ergvein.Wallet.Language

data IPStrings =
  IPParseFailed
  deriving (Eq, Show)

instance LocalizedPrint IPStrings where
  localizedShow l v = case l of
    English -> case v of
      IPParseFailed          -> "Failed to parse IP address"
    Russian -> case v of
      IPParseFailed          -> "IP адрес неверного формата"
