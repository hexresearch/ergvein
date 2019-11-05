module Ergvein.Wallet.Menu(
    menuWidget
  ) where

import Ergvein.Wallet.Elements 
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Ergvein.Wallet.Monad

menuWidget :: MonadFrontBase t m => m ()
menuWidget = divClass "menu-header" $ do
  divClass "menu-wallet-name" $ text "Default wallet"
  divClass "menu-wallet-menu" $ do
    menuIconUrl <- createObjectURL menuIcon
    btnE <- divButton "menu-button" $ imgClass menuIconUrl ""
    pure ()
