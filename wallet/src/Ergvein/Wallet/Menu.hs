module Ergvein.Wallet.Menu(
    headerWidgetDesktop
  , headerWidgetAndroid
  , headerWidgetOnlyBackBtn
  , headerWidgetOnlyLogoutBtn
  ) where

import {-# SOURCE #-} Ergvein.Wallet.Menu.Switcher
import Data.Text (Text)
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu.Types
import Ergvein.Wallet.Monad


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
  backButtonRetract
  divClass "header-wallet-text" $ dynText titleD
  menuBtnE <- divButton "header-button" $ elClassDyn "i" menuButtonIconClassD blank
  let menuButtonIconClassD = menuButtonIconClass <$> menuIsHiddenD
  menuIsHiddenD <- toggle True menuBtnE
  pure menuIsHiddenD

headerAndroid :: MonadFront t m => Dynamic t Text -> m (Event t ())
headerAndroid titleD = divClass "header header-black" $ mdo
  backButtonRetract
  divClass "header-wallet-text" $ dynText titleD
  divButton "header-button" $ elClass "i" "fas fa-bars fa-fw" blank

menuDesktop :: MonadFront t m => Dynamic t Bool -> Maybe (Dynamic t (m ())) -> m ()
menuDesktop menuIsHiddenD thisWidget = do
  let menuClassesD = visibilityClass "menu" <$> menuIsHiddenD
  divClassDyn menuClassesD $ do
    ps <- getPubStorage
    let activeCurrencies = _pubStorage'activeCurrencies ps
    menuButtonsDesktop thisWidget $ case activeCurrencies of
      cur:[] -> Just cur
      _ -> Nothing

menuAndroid :: MonadFront t m => Event t () -> Maybe (Dynamic t (m ())) -> m ()
menuAndroid menuOpenE thisWidget = mdo
  menuIsHiddenD <- holdDyn True $ leftmost [False <$ menuOpenE, True <$ closeMenuE]
  let menuWrapperClassesD = visibilityClass "menu-android-wrapper" <$> menuIsHiddenD
      menuClassesD = visibilityClass "menu-android" <$> menuIsHiddenD
  closeMenuE <- divClassDyn menuWrapperClassesD $ divClassDyn menuClassesD $ do
    ps <- getPubStorage
    let activeCurrencies = _pubStorage'activeCurrencies ps
    menuBtnE <- divClass "menu-android-header" $ divButton "menu-android-close-button header-button" $ elClass "i" "fas fa-times fa-fw" blank
    divClass "menu-android-buttons-wrapper" $ menuButtonsAndroid thisWidget $ case activeCurrencies of
      cur:[] -> Just cur
      _ -> Nothing
    pure menuBtnE
  pure ()

-- | Creates button that looks like left arrow
backButton :: MonadFrontBase t m => Text -> Dynamic t Bool -> (Event t () -> m (Event t ())) -> m ()
backButton classes isHiddenD handler = do
  let backButtonClassesD = visibilityClass classes <$> isHiddenD
  e <- divButton backButtonClassesD $ elClass "i" "fas fa-arrow-left fa-fw" blank
  void $ handler e

-- | Button for going back on widget history
backButtonRetract :: MonadFrontBase t m => m ()
backButtonRetract = do
  stD <- getRetractStack
  backButton "header-button" (null <$> stD) retract

-- | Button for logging out (used on restore page)
backButtonLogout :: MonadFrontBase t m => m ()
backButtonLogout = do
  backButton "header-button" (constDyn False) (\e -> setAuthInfo $ Nothing <$ e)

menuButtonIconClass :: Bool -> Text
menuButtonIconClass True = "fas fa-bars fa-fw"
menuButtonIconClass False = "fas fa-times fa-fw"

-- | Appends "hidden" class to the given classes if the second argument equals True
visibilityClass :: Text -> Bool -> Text
visibilityClass classes True = classes <> " hidden"
visibilityClass classes False = classes

headerWidgetOnlyBackBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyBackBtn = divClass "header" $ do
  backButtonRetract

headerWidgetOnlyLogoutBtn :: MonadFrontBase t m => m ()
headerWidgetOnlyLogoutBtn = divClass "header" $ do
  backButtonLogout

menuButtonsDesktop :: MonadFront t m => Maybe (Dynamic t (m ())) -> Maybe Currency -> m ()
menuButtonsDesktop thisWidget mCur = do
  let menuBtn v = (v <$) <$> clearButton v
  balE <- menuBtn $ maybe MenuBalances MenuSingleBalance mCur
  setE <- menuBtn MenuSettings
  abtE <- menuBtn MenuAbout
  switchE <- menuBtn MenuSwitch
  switchMenu thisWidget $ leftmost [balE, setE, abtE, switchE]

menuButtonsAndroid :: MonadFront t m => Maybe (Dynamic t (m ())) -> Maybe Currency -> m ()
menuButtonsAndroid thisWidget mCur = do
  let menuBtn (v, i) = divButton "menu-android-button" $ do
        elClass "i" (i <> " menu-android-button-icon") blank
        localizedText v
        pure v
  balE <- menuBtn $ (maybe MenuBalances MenuSingleBalance mCur, "fas fa-wallet fa-fw")
  setE <- menuBtn (MenuSettings, "fas fa-cog fa-fw")
  abtE <- menuBtn (MenuAbout, "fas fa-info-circle fa-fw")
  switchE <- menuBtn (MenuSwitch, "fas fa-sign-out-alt fa-fw")
  switchMenu thisWidget $ leftmost [balE, setE, abtE, switchE]
