-- | Page for mnemonic phrase generation
module Ergvein.Wallet.Page.Seed(
    mnemonicWidget
  ) where

import Ergvein.Wallet.Env

mnemonicWidget :: MonadFront t m => m (Event t Text)
mnemonicWidget = pure never
