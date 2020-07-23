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

headerWidgetOnlyBackBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyBackBtn = divClass "header" $ do
  stD <- getRetractStack
  void $ backButton "header-button" $ null <$> stD

headerWidget :: MonadFront t m => Dynamic t Text -> Maybe (Dynamic t (m ())) -> m ()
headerWidget titleD thisWidget = divClass "header-wrapper" $ mdo
  menuIsHiddenD <- header titleD
  menu menuIsHiddenD thisWidget

header :: MonadFront t m => Dynamic t Text -> m (Dynamic t Bool)
header titleD = divClass "header header-black" $ mdo
  stD <- getRetractStack
  backButton "header-button" $ null <$> stD
  divClass "header-wallet-text" $ dynText titleD
  menuBtnE <- divButton "header-button" $ elClassDyn "i" menuButtonIconClassD blank
  let menuButtonIconClassD = menuButtonIconClass <$> menuIsHiddenD
  menuIsHiddenD <- toggle True menuBtnE
  pure menuIsHiddenD

menu :: MonadFront t m => Dynamic t Bool -> Maybe (Dynamic t (m ())) -> m ()
menu menuIsHiddenD thisWidget = do
  let menuClassesD = visibilityClass "menu" <$> menuIsHiddenD
  divClassDyn menuClassesD $ do
    ps <- getPubStorage
    let activeCurrencies = _pubStorage'activeCurrencies ps
    if (L.length activeCurrencies == 1)
      then menuButtons [              MenuNetwork, MenuSettings, MenuAbout, MenuLogs, MenuSwitch] thisWidget
      else menuButtons [MenuBalances, MenuNetwork, MenuSettings, MenuAbout, MenuLogs, MenuSwitch] thisWidget

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
