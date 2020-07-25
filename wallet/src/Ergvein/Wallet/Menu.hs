module Ergvein.Wallet.Menu(
    headerWidgetDesktop
  , headerWidgetAndroid
  , headerWidgetOnlyBackBtn
  ) where

import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
import Data.Text (Text)
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Platform

import qualified Data.List as L

headerWidgetOnlyBackBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyBackBtn = divClass "header" $ do
  stD <- getRetractStack
  void $ backButton "header-button" $ null <$> stD

headerWidgetDesktop :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidgetDesktop titleD thisWidget = divClass "header-wrapper" $ do
  menuIsHiddenD <- headerDesktop titleD
  menuDesktop menuIsHiddenD thisWidget

headerWidgetAndroid :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidgetAndroid titleD thisWidget = do
  menuOpenE <- divClass "header-wrapper" $ headerAndroid titleD
  menuAndroid menuOpenE thisWidget

headerDesktop :: MonadFront t m => Dynamic t Text -> m (Dynamic t Bool)
headerDesktop titleD = divClass "header header-black" $ mdo
  stD <- getRetractStack
  backButton "header-button" $ null <$> stD
  divClass "header-wallet-text" $ dynText titleD
  menuBtnE <- divButton "header-button" $ elClassDyn "i" menuButtonIconClassD blank
  let menuButtonIconClassD = menuButtonIconClass <$> menuIsHiddenD
  menuIsHiddenD <- toggle True menuBtnE
  pure menuIsHiddenD

headerAndroid :: MonadFront t m => Dynamic t Text -> m (Event t ())
headerAndroid titleD = divClass "header header-black" $ mdo
  stD <- getRetractStack
  backButton "header-button" $ null <$> stD
  divClass "header-wallet-text" $ dynText titleD
  divButton "header-button" $ elClass "i" "fas fa-bars fa-fw" blank

menuDesktop :: MonadFront t m => Dynamic t Bool -> Maybe (Dynamic t (m ())) -> m ()
menuDesktop menuIsHiddenD thisWidget = do
  let menuClassesD = visibilityClass "menu" <$> menuIsHiddenD
  divClassDyn menuClassesD $ do
    ps <- getPubStorage
    let activeCurrencies = _pubStorage'activeCurrencies ps
    if (L.length activeCurrencies == 1)
      then menuButtons [              MenuNetwork, MenuSettings, MenuAbout, MenuLogs, MenuSwitch] thisWidget
      else menuButtons [MenuBalances, MenuNetwork, MenuSettings, MenuAbout, MenuLogs, MenuSwitch] thisWidget

menuAndroid :: MonadFront t m => Event t () -> Maybe (Dynamic t (m ())) -> m ()
menuAndroid menuOpenE thisWidget = mdo
  menuIsHiddenD <- holdDyn True $ leftmost [False <$ menuOpenE, True <$ closeMenuE]
  let menuWrapperClassesD = visibilityClass "menu-android-wrapper" <$> menuIsHiddenD
      menuClassesD = visibilityClass "menu-android" <$> menuIsHiddenD
  closeMenuE <- divClassDyn menuWrapperClassesD $ divClassDyn menuClassesD $ do
    ps <- getPubStorage
    let activeCurrencies = _pubStorage'activeCurrencies ps
    menuBtnE <- divClass "menu-android-header" $ divButton "menu-android-close-button header-button" $ elClass "i" "fas fa-times fa-fw" blank
    divClass "menu-android-buttons-wrapper" $ if (L.length activeCurrencies == 1)
      then menuButtonsAndroid [
              (MenuNetwork, "fas fa-network-wired fa-fw")
            , (MenuSettings, "fas fa-cog fa-fw")
            , (MenuAbout, "fas fa-info-circle fa-fw")
            , (MenuLogs, "fas fa-file-alt fa-fw")
            , (MenuSwitch, "fas fa-sign-out-alt fa-fw")
            ] thisWidget
      else menuButtonsAndroid [
              (MenuBalances, "fas fa-wallet fa-fw")
            , (MenuNetwork, "fas fa-network-wired fa-fw")
            , (MenuSettings, "fas fa-cog fa-fw")
            , (MenuAbout, "fas fa-info-circle fa-fw")
            , (MenuLogs, "fas fa-file-alt fa-fw")
            , (MenuSwitch, "fas fa-sign-out-alt fa-fw")
            ] thisWidget
    pure menuBtnE
  pure ()

-- | Button for going back on widget history
backButton :: MonadFrontBase t m => Text -> Dynamic t Bool -> m ()
backButton classes isHiddenD = do
  let backButtonClassesD = visibilityClass classes <$> isHiddenD
  e <- divButton backButtonClassesD $ elClass "i" "fas fa-arrow-left fa-fw" blank
  void $ retract e

menuButtonIconClass :: Bool -> Text
menuButtonIconClass True = "fas fa-bars fa-fw"
menuButtonIconClass False = "fas fa-times fa-fw"

-- | Appends "hidden" class to the given classes if the second argument equals True
visibilityClass :: Text -> Bool -> Text
visibilityClass classes True = classes <> " hidden"
visibilityClass classes False = classes

menuButtons :: MonadFront t m => [MenuItem] -> Maybe (Dynamic t (m ())) -> m ()
menuButtons menuItems thisWidget = do
  btnsEvents <- sequenceA $ menuBtn <$> menuItems
  switchMenu thisWidget $ leftmost btnsEvents
  where menuBtn v = (v <$) <$> clearButton v

menuButtonsAndroid :: MonadFront t m => [(MenuItem, Text)] -> Maybe (Dynamic t (m ())) -> m ()
menuButtonsAndroid menuItems thisWidget = do
  btnsEvents <- sequenceA $ menuBtn <$> menuItems
  switchMenu thisWidget $ leftmost btnsEvents
  where menuBtn (v, i) = divButton "menu-android-button" $ do
          elClass "i" (i <> " menu-android-button-icon") blank
          localizedText v
          pure v
