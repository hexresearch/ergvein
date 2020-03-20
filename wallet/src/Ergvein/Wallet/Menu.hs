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
    backButton $ null <$> stD
    divClass "menu-wallet-name" $ localizedText titleVal -- "Default wallet"
    divButton "menu-header-button menu-dropdown-button" $ elClassDyn "i" menuDropdownButtonIconClassD blank
  dropdownIsHiddenD <- toggle True btnE
  let dropdownClassesD = visibilityClass "menu-dropdown" <$> dropdownIsHiddenD
  let menuDropdownButtonIconClassD = menuDropdownButtonIconClass <$> dropdownIsHiddenD
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

-- | Adds "hidden" class to the given classes if predicate is True
visibilityClass :: Text -> Bool -> Text
visibilityClass c isHidden = c <> " " <> toClass isHidden
  where 
    toClass True = "hidden"
    toClass _    = ""

menuDropdownButtonIconClass :: Bool -> Text
menuDropdownButtonIconClass True = "fas fa-bars fa-fw"
menuDropdownButtonIconClass _    = "fas fa-times fa-fw"
