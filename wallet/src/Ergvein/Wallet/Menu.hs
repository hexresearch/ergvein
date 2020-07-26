module Ergvein.Wallet.Menu(
    headerWidget
  , headerWidgetSimple
  , headerWidgetOnlyBackBtn
  ) where

import Data.Text (Text)
import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad

import qualified Data.List as L

headerWidget :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidget titleVal prevWidget = divClass "header-wrapper" $ mdo
  btnE <- divClass "header" $ do
    stD <- getRetractStack
    backButton "header-button header-back-button" $ null <$> stD
    divClass "header-wallet-text" $ dynText titleVal
    divButton "header-button header-menu-dropdown-button" $ elClassDyn "i" menuDropdownButtonIconClassD blank
  dropdownIsHiddenD <- toggle True btnE
  ps <- getPubStorage
  let dropdownClassesD = visibilityClass "header-menu-dropdown" <$> dropdownIsHiddenD
      menuDropdownButtonIconClassD = menuDropdownButtonIconClass <$> dropdownIsHiddenD
      currencies = _pubStorage'activeCurrencies ps
  headerDropdown prevWidget dropdownClassesD $ case currencies of
    cur:[] -> Just cur
    _ -> Nothing

headerWidgetSimple :: (MonadFront t m, LocalizedPrint a) => a -> Maybe (Dynamic t (m ())) -> m ()
headerWidgetSimple titleVal prevWidget = divClass "header-wrapper" $ mdo
  btnE <- divClass "header" $ do
    stD <- getRetractStack
    backButton "header-button header-back-button" $ null <$> stD
    divClass "header-wallet-text" $ localizedText titleVal -- "Default wallet"
    divButton "header-button header-menu-dropdown-button" $ elClassDyn "i" menuDropdownButtonIconClassD blank
  dropdownIsHiddenD <- toggle True btnE
  ps <- getPubStorage
  let dropdownClassesD = visibilityClass "header-menu-dropdown" <$> dropdownIsHiddenD
      menuDropdownButtonIconClassD = menuDropdownButtonIconClass <$> dropdownIsHiddenD
      currencies = _pubStorage'activeCurrencies ps
  headerDropdown prevWidget dropdownClassesD $ case currencies of
    cur:[] -> Just cur
    _ -> Nothing

headerDropdown :: MonadFront t m => Maybe (Dynamic t (m ())) -> Dynamic t Text -> Maybe Currency -> m ()
headerDropdown prevWidget dropdownClassesD mcur = divClassDyn dropdownClassesD $ do
  let menuBtn v = (v <$) <$> clearButton v
  balE <- menuBtn $ maybe MenuBalances MenuSingleBalance mcur
  netE <- menuBtn MenuNetwork
  setE <- menuBtn MenuSettings
  abtE <- menuBtn MenuAbout
  logE <- menuBtn MenuLogs
  switchE <- menuBtn MenuSwitch
  switchMenu prevWidget $ leftmost [balE, netE, setE, abtE, logE, switchE]

headerWidgetOnlyBackBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyBackBtn = divClass "header-wrapper" $ divClass "header-only-back-btn" $ do
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
