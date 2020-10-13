module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Lens
import Data.List
import Data.Maybe (fromMaybe)
import Reflex.Dom

import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Localization.Util
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Settings.MnemonicExport
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as Map
import qualified Data.Set as S

-- TODO: uncomment commented lines when ERGO is ready
data SubPageSettings
  = GoLanguage
  -- | GoCurrencies
  | GoUnits
  | GoNetwork
  | GoPortfolio
  | GoMnemonicExport Mnemonic
  | GoDns

-- TODO: uncomment commented lines when ERGO is ready
settingsPage :: MonadFront t m => m ()
settingsPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure settingsPage) $ do
    divClass "initial-options grid1" $ do
      let btns = [
              (GoLanguage, STPSButLanguage)
            , (GoNetwork, STPSButNetwork)
            , (GoUnits, STPSButUnits)
            , (GoPortfolio, STPSButPortfolio)
            , (GoDns, STPSButDns)
            ]
      goE' <- fmap leftmost $ flip traverse btns $ \(v,l) -> fmap (v <$) $ outlineButton l
      mnemonicExportBtnE <- outlineButton STPSButMnemonicExport
      goMnemonicExportE <- withWallet $
        ffor mnemonicExportBtnE $ \_ prvStorage -> do
          pure $ GoMnemonicExport $ _prvStorage'mnemonic prvStorage
      let goE = leftmost [ goE', goMnemonicExportE ]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage                -> languagePage
            -- GoCurrencies              -> currenciesPage
            GoNetwork                 -> networkSettingsPage
            GoUnits                   -> unitsPage
            GoPortfolio               -> portfolioPage
            GoMnemonicExport mnemonic -> mnemonicExportPage mnemonic
            GoDns                     -> dnsPage
        , retractablePrev = Just $ pure settingsPage
        }

-- TODO: use dyn settings instead of simple settings <- getSettings
languagePage :: MonadFront t m => m ()
languagePage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure languagePage) languagePageWidget

-- TODO: use dyn settings instead of simple settings <- getSettings
dnsPage :: MonadFront t m => m ()
dnsPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure dnsPage) dnsPageWidget

currenciesPage :: MonadFront t m => m ()
currenciesPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure currenciesPage) $ do
    h3 $ localizedText STPSSetsActiveCurrs
    divClass "initial-options" $ mdo
      activeCursD <- getActiveCursD
      ps <- getPubStorage
      authD <- getAuthInfo
      currListE <- fmap switchDyn $ widgetHoldDyn $ ffor activeCursD $ \currs ->
        selectCurrenciesWidget $ S.toList currs
      void $ uac currListE
      updateAE <- withWallet $ ffor currListE $ \curs prvStr -> do
          auth <- sample . current $ authD
          let authNew = auth & authInfo'storage . storage'pubStorage . pubStorage'activeCurrencies .~ curs
              difC = curs \\ (_pubStorage'activeCurrencies ps)
              mpath = auth ^. authInfo'storage . storage'pubStorage . pubStorage'pathPrefix
              mL = Map.fromList [(currency, mkStore mpath prvStr currency) | currency <- difC ]
              authN2 = authNew & authInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages %~ (Map.union mL)
          pure $ Just $ authN2
      setAuthInfoE <- setAuthInfo updateAE
      storeWallet "currenciesPage" (void $ updated authD)
      showSuccessMsg $ STPSSuccess <$ setAuthInfoE
      pure ()
  where
    uac cE =  updateActiveCurs $ fmap (\cl -> const (S.fromList cl)) $ cE
    mkStore mpath prvStr currency = let
      dpath = extendDerivPath currency <$> mpath
      in CurrencyPubStorage {
        _currencyPubStorage'pubKeystore   = (createPubKeystore $ deriveCurrencyMasterPubKey dpath (_prvStorage'rootPrvKey prvStr) currency)
      , _currencyPubStorage'path          = dpath
      , _currencyPubStorage'transactions  = Map.empty
      , _currencyPubStorage'height        = Nothing
      , _currencyPubStorage'scannedKey    = (Just 0, Just 0)
      , _currencyPubStorage'utxos         = Map.empty
      , _currencyPubStorage'scannedHeight = Nothing
      , _currencyPubStorage'headers       = Map.empty
      , _currencyPubStorage'outgoing      = S.empty
      , _currencyPubStorage'headerSeq     = btcCheckpoints
      }

-- TODO: uncomment commented lines when ERGO is ready
unitsPage :: MonadFront t m => m ()
unitsPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure unitsPage) $ mdo
    cntED <- widgetHold content $ content <$ switchDyn cntED
    pure ()
  where
    content = do
      h3 $ localizedText $ STPSSelectUnitsFor BTC
      ubE <- divClass "initial-options grid1" $ do
        settings <- getSettings
        let setUs = getSettingsUnits settings
        unitBtcE <- unitsDropdown (getUnitBTC setUs) allUnitsBTC
        setE <- updateSettings $ ffor unitBtcE (\ubtc -> settings {settingsUnits = Just $ setUs {unitBTC = Just ubtc}})
        delay 0.1 $ () <$ setE
      pure ubE

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
portfolioPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure portfolioPage) $ do
    h3 $ localizedText STPSSetsPortfolio
    divClass "initial-options" $ mdo
      settings <- getSettings
      let sFC = settingsFiatCurr settings
      divClass "select-currencies-title" $ h4 $ localizedText STPSSetsPortfolioEnable
      portD <- holdDyn (settingsPortfolio settings) $ poke pbtnE $ \_ -> do
        portS <- sampleDyn portD
        pure $ not portS
      pbtnE <- divButton (fmap toggled portD) $ widgetHoldDyn $ ffor portD $ \pS ->
        if pS
          then localizedText CSOn
          else localizedText CSOff
      void $ updateSettings $ ffor (updated portD) (\portS -> settings {settingsPortfolio = portS})
      divClass "select-currencies-title" $ h4 $ localizedText STPSSetsFiatSelect
      fiatE <- fiatDropdown sFC allFiats
      void $ updateSettings $ ffor fiatE (\fiat -> settings {settingsFiatCurr = fiat})
  where
    toggled b = if b
      then "button button-on"
      else "button button-off"

    fiatDropdown val fiats = do
      let fiatD = constDyn val
      initKey <- sample . current $ fiatD
      let listFiatsD = constDyn $ Map.fromList $ fmap (\f -> (f, showt f)) fiats
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated fiatD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- divClass "select-fiat" $ dropdown initKey listFiatsD ddnCfg
      let selD = _dropdown_value dp
      fmap updated $ holdUniqDyn selD
