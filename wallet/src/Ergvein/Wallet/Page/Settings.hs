{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import Data.Function.Flip (flip3)
import Reflex.Host.Class
import Reflex.Dom as RD

import Ergvein.Text
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Widget.GraphPinCode
import Ergvein.Wallet.Wrapper

data SubPageSettings
  = GoLanguage
  | GoPinCode

settingsPage :: MonadFront t m => m ()
settingsPage = do
  let thisWidget = Just $ pure $ settingsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    divClass "initial-options grid1" $ do
      goLangE <- fmap (GoLanguage <$) $ outlineButton STPSButLanguage
--      goPinE  <- fmap (GoPinCode  <$) $ outlineButton STPSButPinCode
      let goE = leftmost [goLangE]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage  -> languagePage
            GoPinCode   -> pinCodePage
        , retractablePrev = thisWidget
        }

languagePage :: MonadFront t m => m ()
languagePage = do
  let thisWidget = Just $ pure $ languagePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSelectLanguage
    divClass "initial-options grid1" $ do
      langD <- getLanguage
      initKey <- sample . current $ langD
      let listLangsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allLanguages
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated langD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listLangsD ddnCfg
      let selD = _dropdown_value dp
      selE <- fmap updated $ holdUniqDyn selD
      widgetHold (pure ()) $ setLanguage <$> selE
      settings <- getSettings
      updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
      pure ()
    pure ()

data GoPinSets
  = GoPinBase
  | GoPinInputCode

pinCodePage :: MonadFront t m => m ()
pinCodePage = do
  let thisWidget = Just $ pure $ pinCodePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    goPinSetsE <- fmap (GoPinBase <$) getPostBuild
    rec goPinE <- fmap switchDyn $ widgetHold (pure never) $
          ffor (leftmost [goPinE, goPinSetsE]) $ \case
            GoPinBase       -> pagePinBase
            GoPinInputCode  -> pagePinInput
    pure ()
    where
      pagePinBase :: MonadFront t m => m (Event t GoPinSets)
      pagePinBase = do
        h3 $ localizedText $ STPSSetsPinCode
        setsPinCodeMb <- fmap settingsPinCode getSettings
        switchE' <- outlineButton $ case setsPinCodeMb of
                      Nothing -> STPSSetsPinOn
                      Just _  -> STPSSetsPinOff
        let switchE = ffor switchE' $ \_ -> case setsPinCodeMb of
                        Nothing -> GoPinInputCode
                        Just _  -> GoPinBase
        let cleanE = fforMaybe switchE $ \case
                    GoPinBase -> Just ()
                    _         -> Nothing
        settings <- getSettings
        updateSettings $ ffor cleanE (\_ -> settings {settingsPinCode = Nothing})
        delay 0.1 switchE

      pagePinInput :: MonadFront t m => m (Event t GoPinSets)
      pagePinInput = do
        h3 $ localizedText $ STPSSetsPinInput
        pinCodeE <- graphPinCode never
        setPinE <- fmap switchDyn $ widgetHold (pure never) $
          ffor pinCodeE $ \PinCode{..} -> do
            elAttr "div" [("style","height: 25px;")] blank
            fmap (unPinCode <$) $ outlineButton STPSSetsPinDoSet
        settings <- getSettings
        updateSettings $ ffor setPinE (\pcv -> settings {settingsPinCode = Just $ showt pcv})
        delay 0.1 $ GoPinBase <$ setPinE
