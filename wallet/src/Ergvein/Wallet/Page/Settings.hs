{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Settings(
    settingsPage
  , torPage
  , currenciesPage
  ) where

import Control.Lens
import Data.Maybe (catMaybes)
import Data.List
import Data.Traversable (for)
import Reflex.Dom
import Reflex.ExternalRef

import Ergvein.Crypto
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Settings.MnemonicExport
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Toggle

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T

data SubPageSettings
  = GoLanguage
  | GoCurrencies
  | GoUnits
  | GoNetwork
  | GoPortfolio
  | GoMnemonicExport
  | GoDns
  | GoNodes
  | GoRbf
  | GoPassword
  | GoDelete

settingsPage :: MonadFront t m => m ()
settingsPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure settingsPage) $ do
    divClass "initial-options grid1" $ do
      let btns = [
              (GoLanguage, STPSButLanguage)
            , (GoNetwork, STPSButNetwork)
            , (GoUnits, STPSButDisplay)
            , (GoCurrencies, STPSButActiveCurrs)
            , (GoDns, STPSButDns)
            , (GoNodes, STPSButNodes)
            , (GoRbf, STPSButRbf)
            , (GoPassword, STPSButSetPass)
            , (GoDelete, STPSButDeleteWallet)
            , (GoMnemonicExport, STPSButMnemonicExport)
            ]
      goE <- fmap leftmost $ for btns $ \(v,l) -> (v <$) <$> outlineButton l
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage        -> languagePage
            GoCurrencies      -> currenciesPage
            GoNetwork         -> networkSettingsPage
            GoUnits           -> unitsPage
            GoMnemonicExport  -> mnemonicExportPage
            GoNodes           -> btcNodesPage
            GoRbf             -> rbfPage
            GoDns             -> dnsPage
            GoPassword        -> passwordChangePage
            GoDelete          -> deleteWalletPage
        , retractablePrev = Just $ pure settingsPage
        }

passwordChangePage :: MonadFront t m => m ()
passwordChangePage = do
  passE <- changePasswordWidget
  walletInfoD <- getWalletInfo
  eaibE <- withWallet $ ffor passE $ \(pass, b) prv -> do
    ai <- sampleDyn walletInfoD
    encryptPrvStorageResult <- encryptPrvStorage prv pass
    case encryptPrvStorageResult of
      Left err -> pure $ Left $ CreateStorageAlert err
      Right prve -> case passwordToECIESPrvKey pass of
        Left _ -> pure $ Left GenerateECIESKeyAlert
        Right k -> pure $ Right $ (,b) $ ai
          & walletInfo'storage . storage'encryptedPrvStorage .~ prve
          & walletInfo'eciesPubKey .~ toPublic k
          & walletInfo'isPlain .~ (pass == "")
  aibE <- handleDangerMsg eaibE
  when isAndroid $ performEvent_ $ ffor aibE $ \(ai,b) -> do
    let fpath = "meta_wallet_" <> T.replace " " "_" (_walletInfo'login ai)
    storeValue fpath b True
  doneE <- storeWallet "passwordChangePage" =<< setWalletInfo (fmap (Just . fst) aibE)
  void $ nextWidget $ ffor doneE $ const $ Retractable{
      retractableNext = settingsPage
    , retractablePrev = Nothing
    }

languagePage :: MonadFront t m => m ()
languagePage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure languagePage) languagePageWidget

dnsPage :: MonadFront t m => m ()
dnsPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure dnsPage) dnsPageWidget

torPage :: MonadFront t m => m ()
torPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure torPage) torPageWidget

currenciesPage :: MonadFront t m => m ()
currenciesPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure currenciesPage) $ do
    h3 $ localizedText STPSSetsActiveCurrs
    divClass "initial-options" $ mdo
      activeCursD <- getActiveCursD
      ps <- getPubStorage
      authD <- getWalletInfo
      currListE <- fmap switchDyn $ networkHoldDyn $ ffor activeCursD $ \currs ->
        selectCurrenciesWidget $ S.toList currs
      void $ uac currListE
      updateAE <- withWallet $ ffor currListE $ \curs prvStr -> do
          auth <- sample . current $ authD
          let authNew = auth & walletInfo'storage . storage'pubStorage . pubStorage'activeCurrencies .~ curs
              difC = curs \\ _pubStorage'activeCurrencies ps
              mpath = auth ^. walletInfo'storage . storage'pubStorage . pubStorage'pathPrefix
              mL = Map.fromList [(currency, mkStore mpath prvStr currency) | currency <- difC ]
              authN2 = authNew & walletInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages %~ Map.union mL
          pure $ Just authN2
      setWalletInfoE <- setWalletInfo updateAE
      void $ storeWallet "currenciesPage" (void $ updated authD)
      showSuccessMsg $ STPSSuccess <$ setWalletInfoE
      pure ()
  where
    uac cE =  updateActiveCurs $ const . S.fromList <$> cE
    mkStore mpath prvStr currency = createCurrencyPubStorage mpath (_prvStorage'rootPrvKey prvStr) (filterStartingHeight currency) currency

unitsPage :: MonadFront t m => m ()
unitsPage = do
  title <- localized STPSTitle
  wrapper False title (Just $ pure unitsPage) $ do
    divClass "" $ do
      btcUnitsSettingsWidget
      fiatUnitsSettingsWidget
      fiatBalanceSettingsWidget
      fiatRateSettingsWidget
    pure ()

  where

    btcUnitsSettingsWidget :: MonadFront t m => m ()
    btcUnitsSettingsWidget = do
      settingsD <- getSettingsD
      initUnit <- getSettingsUnitBtc
      unitBtcE <- divClass "fiat-settings" $ do
        spanClass "fiat-settings-label pr-2" $ localizedText $ STPSSelectUnitsFor BTC
        unitsDropdown initUnit allUnitsBTC
      void $ updateSettings $ poke unitBtcE $ \unit -> do
        settings <- sampleDyn settingsD
        pure $ setUnitSetting settings unit

    setUnitSetting :: Settings -> UnitBTC -> Settings
    setUnitSetting s val =
      let oldSettings = settingsCurrencySpecific s
          oldBtcSettings = getBtcSettings s
      in s {settingsCurrencySpecific = Map.insert BTC (SettingsBtc $ oldBtcSettings {btcSettings'units = val}) oldSettings}

    fiatUnitsSettingsWidget :: MonadFront t m => m ()
    fiatUnitsSettingsWidget = do
      settingsD <- getSettingsD
      initSettings <- sampleDyn settingsD
      let initFiat = settingsFiatCurr initSettings
      symbE <- divClass "fiat-settings" $ do
        spanClass "fiat-settings-label pr-2" $ localizedText STPSSetsFiatSelect
        unitsDropdown initFiat allFiats
      void $ updateSettings $ poke symbE $ \fiat -> do
        settings <- sampleDyn settingsD
        pure $ settings {settingsFiatCurr = fiat}

    fiatBalanceSettingsWidget :: MonadFront t m => m ()
    fiatBalanceSettingsWidget = do
      settingsD <- getSettingsD
      initSettings <- sampleDyn settingsD
      let showFiatBalance = settingsShowFiatBalance initSettings
      selD <- divClass "fiat-settings" $ do
        spanClass "fiat-settings-label pr-2" $ localizedText STPSSetsShowFiatBalance
        toggler $ pure showFiatBalance
      void $ updateSettings $ poke (updated selD) $ \v -> do
        settings <- sampleDyn settingsD
        pure $ settings {settingsShowFiatBalance = v}

    fiatRateSettingsWidget :: MonadFront t m => m ()
    fiatRateSettingsWidget = do
      settingsD <- getSettingsD
      initSettings <- sampleDyn settingsD
      let showFiatRate = settingsShowFiatRate initSettings
      selD <- divClass "fiat-settings" $ do
        spanClass "fiat-settings-label pr-2" $ localizedText STPSSetsShowFiatRate
        toggler $ pure showFiatRate
      void $ updateSettings $ poke (updated selD) $ \v -> do
        settings <- sampleDyn settingsD
        pure $ settings {settingsShowFiatRate = v}
    
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
      updated <$> holdUniqDyn selD

-- portfolioPage :: MonadFront t m => m ()
-- portfolioPage = do
--   title <- localized STPSTitle
--   wrapper True title (Just $ pure portfolioPage) $ do
--     h3 $ localizedText STPSSetsPortfolio
--     divClass "initial-options" $ mdo
--       settings <- getSettings
--       let sFC = fromMaybe USD $ settingsFiatCurr settings
--       divClass "select-currencies-title" $ h4 $ localizedText STPSSetsPortfolioEnable
--       portD <- holdDyn (settingsPortfolio settings) $ poke pbtnE $ \_ -> do
--         portS <- sampleDyn portD
--         pure $ not portS
--       pbtnE <- divButton (fmap toggled portD) $ networkHoldDyn $ ffor portD $ \pS ->
--         if pS
--           then localizedText CSOn
--           else localizedText CSOff
--       void $ updateSettings $ ffor (updated portD) (\portS -> settings {settingsPortfolio = portS})
--       divClass "select-currencies-title" $ h4 $ localizedText STPSSetsFiatSelect
--       fiatE <- fiatDropdown sFC allFiats
--       void $ updateSettings $ ffor fiatE (\fiat -> settings {settingsFiatCurr = Just fiat})
--   where
--     toggled b = if b
--       then "button button-on button-currency"
--       else "button button-off button-currency"

--     fiatDropdown val fiats = do
--       let fiatD = constDyn val
--       initKey <- sample . current $ fiatD
--       let listFiatsD = constDyn $ Map.fromList $ fmap (\f -> (f, showt f)) fiats
--           ddnCfg = DropdownConfig {
--                 _dropdownConfig_setValue   = updated fiatD
--               , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
--               }
--       dp <- divClass "select-fiat" $ dropdown initKey listFiatsD ddnCfg
--       let selD = _dropdown_value dp
--       updated <$> holdUniqDyn selD

btcNodesPage :: MonadFront t m => m ()
btcNodesPage = do
  title <- localized STPSButNodes
  wrapper False title (Just $ pure btcNodesPage) $ do
    conmapD <- getNodeConnectionsD
    void $ lineOption $ networkHoldDyn $ ffor conmapD $ \cm -> do
      let btcNodes = maybe [] Map.elems $ DM.lookup BtcTag cm
      btcNetworkWidget btcNodes
      for_ btcNodes $ \node -> do
        let offclass = [("class", "mt-a mb-a indexer-offline")]
        let onclass = [("class", "mt-a mb-a indexer-online")]
        let clsD = (\b -> if b then onclass else offclass) <$> nodeconIsUp node
        divClass "network-name" $ do
          let addr = nodeconUrl node
          (e,_) <- elAttr' "span" [("class", "mt-a mb-a mr-1")] $ elClass "i" "fas fa-times" $ pure ()
          let closeE = (addr, NodeMsgClose) <$ domEvent Click e
          postNodeMessage BTC closeE
          elDynAttr "span" clsD $ elClass "i" "fas fa-circle" $ pure ()
          divClass "mt-a mb-a network-name-txt" $ text $ showt addr
        pure ()
  pure ()

btcNetworkWidget :: MonadFront t m => [NodeBtc t] -> m ()
btcNetworkWidget nodes = do
  let activeND = fmap (length . filter id) $ sequence $ nodeconIsUp <$> nodes
  valueOptionDyn $ NPSActiveNum <$> activeND
  descrOption $ NPSNodesNum $ length nodes
  labelHorSep

rbfPage :: MonadFront t m => m ()
rbfPage = do
  title <- localized STPSButRbf
  wrapper False title (Just $ pure rbfPage) $ do
    settings <- getSettings
    let initVal = btcSettings'sendRbfByDefault $ getBtcSettings settings
    initValD <- holdDyn initVal never
    valD <- divClass "fiat-settings" $ do
      spanClass "fiat-settings-label pr-2" $ localizedText STPSEnableRbfByDefault
      toggler initValD
    selE <- updated <$> holdUniqDyn valD
    updE <- updateSettings $ ffor selE (setRbfSetting settings)
    showSuccessMsg $ STPSSuccess <$ updE
    pure ()
  where
    setRbfSetting :: Settings -> Bool -> Settings
    setRbfSetting s val =
      let oldSettings = settingsCurrencySpecific s
          oldBtcSettings = getBtcSettings s
      in s {settingsCurrencySpecific = Map.insert BTC (SettingsBtc $ oldBtcSettings {btcSettings'sendRbfByDefault = val}) oldSettings}

deleteWalletPage :: MonadFront t m => m ()
deleteWalletPage = do
  title <- localized DWSTitle
  void $ wrapper True title (Just $ pure deleteWalletPage) $ do
    h3 $ localizedText DWSTitle
    workflow stageOne
  where
    buttonsRow yesLbl = divClass "mt-1" $ do
      goE <- buttonClass "button button-outline btn-color-red mr-1" yesLbl
      backE <- outlineButton DWSBtnNo
      void $ nextWidget $ ffor backE $ const $ Retractable settingsPage Nothing
      pure goE
    stageOne = Workflow $ do
      h4 $ localizedText DWSWarn1
      nextE <- buttonsRow DWSBtnYes
      pure ((), stageTwo <$ nextE)
    stageTwo = Workflow $ do
      h4 $ localizedText DWSWarn2
      h5 $ localizedText DWSWarn2Desc
      nextE <- buttonsRow DWSBtnYes
      pure ((), stageThree <$ nextE)
    stageThree = Workflow $ do
      h4 $ localizedText DWSWarn3
      reqPassE <- buttonsRow DWSBtnPass
      nextE <- withWallet $ const (pure ()) <$ reqPassE
      pure ((), stageFour <$ nextE)
    stageFour = Workflow $ do
      h4 $ localizedText DWSFinStage
      delE <- buttonsRow DWSBtnYes
      login <- fmap _walletInfo'login . sampleDyn =<< getWalletInfo
      let walletName = "wallet_" <> T.replace " " "_" login
          backupName = "backup_" <> walletName
      doneE <- performEvent $ ffor delE $ const $ do
        deleteStoredFile walletName
        deleteStoredFile backupName
        deleteStoredFile ".last-wallet"
      void $ setWalletInfo $ Nothing <$ doneE
      pure ((), never)

lineOption :: MonadFront t m => m a -> m a
lineOption = divClass "network-wrapper"

descrOption :: (MonadFront t m, LocalizedPrint a) => a -> m ()
descrOption = (>>) elBR . divClass "network-descr" . localizedText

valueOptionDyn, descrOptionDyn :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
descrOptionDyn v = getLanguage >>= \langD -> (>>) elBR (divClass "network-descr" $ dynText $ ffor2 langD v localizedShow)

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-lb")] blank
elBR = el "br" blank
