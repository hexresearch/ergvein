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
    stD <- getRetractStack
    backButton $ (not . null) <$> stD
    divClass "menu-wallet-name" $ localizedText titleVal -- "Default wallet"
    divButton "menu-header-button menu-dropdown-button" $ elClassDyn "i" menuDropdownButtonIconClassD blank
  dropdownVisibleD <- toggle False btnE
  let dropdownClassesD = visibilityClass "menu-dropdown" <$> dropdownVisibleD
  let menuDropdownButtonIconClassD = menuDropdownButtonIconClass <$> dropdownVisibleD
  divClassDyn dropdownClassesD $ do
    let menuBtn v = (v <$) <$> clearButton v
    balE <- menuBtn MenuBalances
    netE <- menuBtn MenuNetwork
    setE <- menuBtn MenuSettings
    abtE <- menuBtn MenuAbout
    logE <- menuBtn MenuLogs
    switchE <- menuBtn MenuSwitch
    switchMenu prevWidget $ leftmost [balE, netE, setE, abtE, logE, switchE]

-- | Button for going back on widget history
backButton :: MonadFrontBase t m => Dynamic t Bool -> m ()
backButton visibilityD = do
  let backButtonVisibleD = visibilityClass "menu-header-button menu-back-button" <$> visibilityD
  e <- divButton backButtonVisibleD $ elClass "i" "fas fa-arrow-left fa-fw" blank
  void $ retract e

visibilityClass :: Text -> Bool -> Text
visibilityClass c isVisible = c <> " " <> toClass isVisible
  where 
    toClass True = "visible"
    toClass _    = ""

menuDropdownButtonIconClass :: Bool -> Text
menuDropdownButtonIconClass True = "fas fa-times fa-fw"
menuDropdownButtonIconClass _    = "fas fa-bars fa-fw"
