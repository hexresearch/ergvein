module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class
import Data.Function.Flip (flip3)
import Data.Maybe (fromMaybe)
import Data.Time
import Reflex.Dom
import Reflex.Dom as RD
import Reflex.Host.Class

import Ergvein.Text
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Monad.Auth
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Widget.GraphPinCode
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as Map
import qualified Data.Set as S

data SubPageSettings
  = GoLanguage
  | GoCurrencies
  | GoUnits
  | GoNetwork
  | GoPortfolio

settingsPage :: MonadFront t m => m ()
settingsPage = wrapper STPSTitle (Just $ pure settingsPage) True $ do
  divClass "initial-options grid1" $ do
    goLangE   <- fmap (GoLanguage   <$) $ outlineButton STPSButLanguage
    goCurrE   <- fmap (GoCurrencies <$) $ outlineButton STPSButActiveCurrs
    goNetE    <- fmap (GoNetwork    <$) $ outlineButton STPSButNetwork
    goUnitsE  <- fmap (GoUnits      <$) $ outlineButton STPSButUnits
    goUnitsE  <- fmap (GoPortfolio  <$) $ outlineButton STPSButPortfolio
    let goE = leftmost [goLangE, goCurrE, goNetE, goUnitsE]
    void $ nextWidget $ ffor goE $ \spg -> Retractable {
        retractableNext = case spg of
          GoLanguage   -> languagePage
          GoCurrencies -> currenciesPage
          GoNetwork    -> networkSettingsPage
          GoUnits      -> unitsPage
          GoPortfolio  -> portfolioPage
      , retractablePrev = Just $ pure settingsPage
      }

languagePage :: MonadFront t m => m ()
languagePage = wrapper STPSTitle (Just $ pure languagePage) True $ do
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
    updE <- updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
    showSuccessMsg $ STPSSuccess <$ updE
  pure ()

currenciesPage :: MonadFront t m => m ()
currenciesPage = wrapper STPSTitle (Just $ pure currenciesPage) True $ do
  h3 $ localizedText STPSSetsActiveCurrs
  divClass "initial-options" $ mdo
    activeCursD <- getActiveCursD
    ps <- getPubStorage
    authD <- getAuthInfo
    void $ widgetHoldDyn $ ffor activeCursD $ \currs -> do
      currListE <- selectCurrenciesWidget $ S.toList currs
      uac currListE
      let updatedAuthE = flip pushAlways currListE $ \curs -> do
              auth <- sample . current $ authD
              pure $ Just $ auth
                & authInfo'storage . storage'pubStorage . pubStorage'activeCurrencies .~ curs
      setAuthInfoE <- setAuthInfo updatedAuthE
      storeWallet (void $ updated authD)
      showSuccessMsg $ STPSSuccess <$ setAuthInfoE
    pure ()
    where
      uac cE =  updateActiveCurs $ fmap (\cl -> const (S.fromList cl)) $ cE



unitsPage :: MonadFront t m => m ()
unitsPage = wrapper STPSTitle (Just $ pure unitsPage) True $ mdo
  cntED <- widgetHold content $ content <$ switchDyn cntED
  pure ()
  where
    content = do
      h3 $ localizedText $ STPSSelectUnitsFor BTC
      ubE <- divClass "initial-options" $ do
        settings <- getSettings
        let setUs = getSettingsUnits settings
        unitBtcE <- unitsDropdown (getUnitBTC setUs) allUnitsBTC
        updateSettings $ ffor unitBtcE (\ubtc -> settings {settingsUnits = Just $ setUs {unitBTC = Just ubtc}})
        delay 0.1 (() <$ unitBtcE)
      h3 $ localizedText $ STPSSelectUnitsFor ERGO
      ueE <- divClass "initial-options" $ do
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


portfolioPage :: MonadFront t m => m ()
portfolioPage = wrapper STPSTitle (Just $ pure currenciesPage) True $ do
  h3 $ localizedText STPSSetsPortfolio
  divClass "initial-options" $ mdo
    settings <- getSettings
    let sFC = settingsFiatCurr settings
    let sP  = settingsPortfolio settings
    divClass "select-currencies-title" $ h4 $ localizedText STPSSetsPortfolioEnable
    portD <- holdDyn (settingsPortfolio settings) $ poke pbtnE $ \_ -> do
       portS <- sampleDyn portD
       pure $ not portS
    pbtnE <- divButton (fmap toggled portD) $ widgetHoldDyn $ ffor portD $ \pS ->
      if pS
        then localizedText CSOn
        else localizedText CSOff
    updateSettings $ ffor (updated portD) (\portS -> settings {settingsPortfolio = portS})
    divClass "select-currencies-title" $ h4 $ localizedText STPSSetsFiatSelect
    fiatE <- fiatDropdown sFC allFiats
    updateSettings $ ffor fiatE (\fiat -> settings {settingsFiatCurr = fiat})
    pure ()
  where
    toggled b = if b
      then "button button-on"
      else "button button-off"

    fiatDropdown val allFiats = do
      let fiatD = constDyn val
      initKey <- sample . current $ fiatD
      let listFiatsD = constDyn $ Map.fromList $ fmap (\f -> (f, showt f)) allFiats
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated fiatD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- divClass "select-fiat" $ dropdown initKey listFiatsD ddnCfg
      let selD = _dropdown_value dp
      fmap updated $ holdUniqDyn selD
