module Ergvein.Types.Restore(
    WalletSource(..)
  ) where

import GHC.Generics

data WalletSource =
    WalletGenerated -- ^ Wallet was generated and we don't need to scan all history
  | WalletRestored -- ^ Wallet was restored from seed and we need to scan all history
  deriving (Show, Eq, Generic)
