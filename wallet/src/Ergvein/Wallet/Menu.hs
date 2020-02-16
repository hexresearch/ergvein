module Ergvein.Wallet.Menu(
    menuWidget
  ) where

import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad

menuWidget :: (MonadFront t m, LocalizedPrint a) => a -> Maybe (Dynamic t (m ())) -> m ()
menuWidget titleVal prevWidget = divClass "menu-header" $ do
  divClass "menu-wallet-name" $ localizedText titleVal -- "Default wallet"
  divClass "menu-wallet-menu" $ do
    menuIconUrl <- createObjectURL menuIcon
    divClass "menu-dropdown-wrapper" $ do
      btnE <- divButton "menu-button" $ imgClass menuIconUrl ""
      divClass "menu-dropdown" $ do
        let menuBtn v = (v <$) <$> clearButton v
        balE <- menuBtn MenuBalances
        netE <- menuBtn MenuNetwork
        setE <- menuBtn MenuSettings
        abtE <- menuBtn MenuAbout
        logE <- menuBtn MenuLogs
        switchE <- menuBtn MenuSwitch
        switchMenu prevWidget $ leftmost [balE, netE, setE, abtE, logE, switchE]
