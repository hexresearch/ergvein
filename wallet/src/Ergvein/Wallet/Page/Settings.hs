module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Reflex.Dom

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

data SubPageSettings
  = GoLanguage
  | GoPinCode
  | GoUnits

settingsPage :: MonadFront t m => m ()
settingsPage = do
  let thisWidget = Just $ pure $ settingsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    divClass "initial-options grid1" $ do
      goLangE   <- fmap (GoLanguage <$) $ outlineButton STPSButLanguage
      goPinE    <- fmap (GoPinCode  <$) $ outlineButton STPSButPinCode
      goUnitsE  <- fmap (GoUnits    <$) $ outlineButton STPSButUnits
      let goE = leftmost [goLangE, goPinE, goUnitsE]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage  -> languagePage
            GoPinCode   -> pinCodePage
            GoUnits     -> unitsPage
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

pinCodePage :: MonadFront t m => m ()
pinCodePage = do
  let thisWidget = Just $ pure $ pinCodePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSetsPinCode
    divClass "initial-options grid1" $ do
      pure ()
    pure ()

instance LocalizedPrint UnitBTC where
  localizedShow _ v = case v of
    BTC_BTC     -> "BTC"
    BTC_mBTC    -> "mBTC"
    BTC_uBTC    -> "uBTC"
    BTC_satoshi -> "satoshi"

instance LocalizedPrint UnitERGO where
  localizedShow _ v = case v of
    ERGO_ERGO -> "ERGO"

unitsPage :: MonadFront t m => m ()
unitsPage = do
  let thisWidget = Just $ pure $ unitsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSelectUnitsFor BTC
    divClass "initial-options grid1" $ do
      settings <- getSettings
      let setUs = getSettingsUnits settings
      unitBtcE <- unitsDropdown (getUnitBTC setUs) allUnitsBTC
      updateSettings $ ffor unitBtcE (\ubtc -> settings {settingsUnits = Just $ setUs {unitBTC = Just ubtc}})
      pure ()
    h3 $ localizedText $ STPSSelectUnitsFor ERGO
    divClass "initial-options grid1" $ do
      settings <- getSettings
      let setUs = getSettingsUnits settings
      unitErgoE <- unitsDropdown (getUnitERGO setUs) allUnitsERGO
      updateSettings $ ffor unitErgoE (\uergo -> settings {settingsUnits = Just $ setUs {unitERGO = Just uergo}})
      pure ()
    pure ()
  where
    unitsDropdown val allUnits = do
      langD <- getLanguage
      let unitD = constDyn val
      initKey <- sample . current $ unitD
      let listUnitsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allUnits
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated unitD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listUnitsD ddnCfg
      let selD = _dropdown_value dp
      fmap updated $ holdUniqDyn selD

    getSettingsUnits = fromMaybe defUnits . settingsUnits

    getUnitBTC Units{..} = fromMaybe defUnitBTC unitBTC
    getUnitERGO Units{..} = fromMaybe defUnitERGO unitERGO
