module Ergvein.Wallet.Menu(
    headerWidget
  , headerWidgetOnlyBackBtn
  ) where

import Data.Text (Text)
import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad

import qualified Data.List as L

headerWidget :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidget titleVal prevWidget = mdo
  btnE <- divClass "header header-black" $ do
    stD <- getRetractStack
    backButton "header-button header-back-button" $ null <$> stD
    divClass "header-wallet-text" $ dynText titleVal
    divButton "header-button header-menu-dropdown-button" $ elClassDyn "i" menuDropdownButtonIconClassD blank
  dropdownIsHiddenD <- toggle True btnE
  ps <- getPubStorage
  let dropdownClassesD = visibilityClass "header-menu-dropdown" <$> dropdownIsHiddenD
      menuDropdownButtonIconClassD = menuDropdownButtonIconClass <$> dropdownIsHiddenD
      currencies = _pubStorage'activeCurrencies ps
  divClassDyn dropdownClassesD $ do
    let menuBtn v = (v <$) <$> clearButton v
    if (L.length currencies == 1)
      then do
        netE <- menuBtn MenuNetwork
        setE <- menuBtn MenuSettings
        abtE <- menuBtn MenuAbout
        logE <- menuBtn MenuLogs
        switchE <- menuBtn MenuSwitch
        switchMenu prevWidget $ leftmost [netE, setE, abtE, logE, switchE]
      else do
        balE <- menuBtn MenuBalances
        netE <- menuBtn MenuNetwork
        setE <- menuBtn MenuSettings
        abtE <- menuBtn MenuAbout
        logE <- menuBtn MenuLogs
        switchE <- menuBtn MenuSwitch
        switchMenu prevWidget $ leftmost [balE, netE, setE, abtE, logE, switchE]

headerWidgetOnlyBackBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyBackBtn = divClass "header" $ do
  stD <- getRetractStack
  void $ backButton "header-button header-back-button" $ null <$> stD

-- | Button for going back on widget history
backButton :: MonadFrontBase t m => Text -> Dynamic t Bool -> m ()
backButton c visibilityD = do
  let backButtonVisibleD = visibilityClass c <$> visibilityD
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
