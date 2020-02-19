module Ergvein.Wallet.Menu(
    menuWidget
  ) where

import Data.Text (Text)
import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad

menuWidget :: (MonadFront t m, LocalizedPrint a) => a -> Maybe (Dynamic t (m ())) -> m ()
menuWidget titleVal prevWidget = divClass "menu-wrapper" $ mdo
  btnE <- divClass "menu-header" $ do
    divClass "menu-wallet-name" $ localizedText titleVal -- "Default wallet"
    divButton "menu-button" $ elClassDyn "i" menuButtonIconClassD blank
  dropdownVisibleD <- toggle False btnE
  let dropdownClassesD = dropdownClasses <$> dropdownVisibleD
  let menuButtonIconClassD = menuButtonIconClass <$> dropdownVisibleD
  divClassDyn dropdownClassesD $ do
    let menuBtn v = (v <$) <$> clearButton v
    balE <- menuBtn MenuBalances
    netE <- menuBtn MenuNetwork
    setE <- menuBtn MenuSettings
    abtE <- menuBtn MenuAbout
    logE <- menuBtn MenuLogs
    switchE <- menuBtn MenuSwitch
    switchMenu prevWidget $ leftmost [balE, netE, setE, abtE, logE, switchE]

dropdownClasses :: Bool -> Text
dropdownClasses v = "menu-dropdown " <> visible v
  where 
    visible True = "visible"
    visible _    = ""

menuButtonIconClass :: Bool -> Text
menuButtonIconClass True = "fas fa-times fa-fw"
menuButtonIconClass _ = "fas fa-bars fa-fw"
