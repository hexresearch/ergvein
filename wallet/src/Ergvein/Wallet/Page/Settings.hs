module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Time
import Data.Function.Flip (flip3)
import Reflex.Host.Class
import Reflex.Dom as RD
import Data.Maybe (fromMaybe)
import Reflex.Dom

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Widget.GraphPinCode
import Ergvein.Wallet.Wrapper

data SubPageSettings
  = GoLanguage
  | GoCurrencies
  | GoUnits

settingsPage :: MonadFront t m => m ()
settingsPage = do
  let thisWidget = Just $ pure $ settingsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    divClass "initial-options grid1" $ do
      goLangE   <- fmap (GoLanguage <$) $ outlineButton STPSButLanguage
      goCurrE   <- fmap (GoCurrencies <$) $ outlineButton STPSButActiveCurrs
      goUnitsE  <- fmap (GoUnits    <$) $ outlineButton STPSButUnits
      let goE = leftmost [goLangE, goCurrE, goUnitsE]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage   -> languagePage
            GoCurrencies -> currenciesPage
            GoUnits      -> unitsPage
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
      let listLangsD = ffor langD $ \l -> Map.fromList $ fmap (\v -> (v, localizedShow l v)) allLanguages
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

currenciesPage :: MonadFront t m => m ()
currenciesPage = do
  let thisWidget = Just $ pure $ currenciesPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSetsActiveCurrs
    divClass "initial-options grid1" $ do
      s <- getSettings
      anon_name <- getWalletName
      currListE <- selectCurrenciesWidget $ getActiveCurrencies anon_name s
      updateSettings $ ffor currListE $ \curs -> s {settingsActiveCurrencies = acSet anon_name s curs}
  where
    getActiveCurrencies name s = fromMaybe allCurrencies $ Map.lookup name $ activeCurrenciesMap $ settingsActiveCurrencies s
    acSet l s curs = ActiveCurrencies $ Map.insert l curs $ activeCurrenciesMap $ settingsActiveCurrencies s

unitsPage :: MonadFront t m => m ()
unitsPage = do
  let thisWidget = Just $ pure $ unitsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ mdo
    cntED <- widgetHold content $ content <$ switchDyn cntED
    pure ()
  where
    content = do
      h3 $ localizedText $ STPSSelectUnitsFor BTC
      ubE <- divClass "initial-options grid1" $ do
        settings <- getSettings
        let setUs = getSettingsUnits settings
        unitBtcE <- unitsDropdown (getUnitBTC setUs) allUnitsBTC
        updateSettings $ ffor unitBtcE (\ubtc -> settings {settingsUnits = Just $ setUs {unitBTC = Just ubtc}})
        delay 0.1 (() <$ unitBtcE)
      h3 $ localizedText $ STPSSelectUnitsFor ERGO
      ueE <- divClass "initial-options grid1" $ do
        settings <- getSettings
        let setUs = getSettingsUnits settings
        unitErgoE <- unitsDropdown (getUnitERGO setUs) allUnitsERGO
        updateSettings $ ffor unitErgoE (\uergo -> settings {settingsUnits = Just $ setUs {unitERGO = Just uergo}})
        delay 0.1 (() <$ unitErgoE)
      pure $ leftmost [ubE, ueE]

    unitsDropdown val allUnits = do
      langD <- getLanguage
      let unitD = constDyn val
      initKey <- sample . current $ unitD
      let listUnitsD = ffor langD $ \l -> Map.fromList $ fmap (\v -> (v, localizedShow l v)) allUnits
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated unitD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listUnitsD ddnCfg
      let selD = _dropdown_value dp
      fmap updated $ holdUniqDyn selD

    getSettingsUnits = fromMaybe defUnits . settingsUnits
