{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE CPP #-}

module Ergvein.Wallet.Page.Settings(
    settingsPage
  , torPage
  , currenciesPage
  ) where

import Control.Lens
import Data.List
import Data.Maybe (fromMaybe, catMaybes)
import Reflex.Dom
import Reflex.ExternalRef

import Ergvein.Crypto
import Ergvein.Text
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Toggle
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization
import Ergvein.Wallet.Monad
import Sepulcas.Native
import Ergvein.Wallet.Navbar
import Ergvein.Wallet.Page.Currencies
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Settings.MnemonicExport
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Dependent.Map as DM

-- TODO: uncomment commented lines when ERGO is ready
data SubPageSettings
  = GoLanguage
  -- | GoCurrencies
  | GoUnits
  | GoNetwork
  | GoPortfolio
  | GoMnemonicExport
  | GoDns
  | GoNodes
  | GoPassword
  | GoDelete
  | GoRbf

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
            -- , (GoPortfolio, STPSButPortfolio)
            , (GoDns, STPSButDns)
            , (GoNodes, STPSButNodes)
            , (GoPassword, STPSButSetPass)
            , (GoDelete, STPSButDeleteWallet)
            , (GoMnemonicExport, STPSButMnemonicExport)
            , (GoRbf, STPSButRbf)
            ]
      goE <- fmap leftmost $ flip traverse btns $ \(v,l) -> fmap (v <$) $ outlineButton l
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage        -> languagePage
            -- GoCurrencies   -> currenciesPage
            GoNetwork         -> networkSettingsPage
            GoUnits           -> unitsPage
            GoPortfolio       -> portfolioPage
            GoMnemonicExport  -> mnemonicExportPage
            GoDns             -> dnsPage
            GoNodes           -> btcNodesPage
            GoPassword        -> passwordChangePage
            GoDelete          -> deleteWalletPage
            GoRbf             -> rbfPage
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
  pure ()

btcNodesPage :: MonadFront t m => m ()
btcNodesPage = do
  title <- localized STPSButNodes
  wrapper False title (Just $ pure $ btcNodesPage) $ do
    conmapD <- getNodeConnectionsD
    void $ lineOption $ networkHoldDyn $ ffor conmapD $ \cm -> do
      let btcNodes = maybe [] Map.elems $ DM.lookup BtcTag cm
      btcNetworkWidget btcNodes
      void $ flip traverse btcNodes $ \node -> do
        let offclass = [("class", "mt-a mb-a indexer-offline")]
        let onclass = [("class", "mt-a mb-a indexer-online")]
        let clsD = fmap (\b -> if b then onclass else offclass) $ nodeconIsUp node
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
  infosD <- fmap sequence $ traverse externalRefDynamic $ nodeconStatus <$> nodes
  let activeND = fmap (length . filter id) $ sequence $ nodeconIsUp <$> nodes
      sumLatD  = fmap (sum . fmap nodestatLat . catMaybes) infosD
      avgLatD  = (\a b -> if b == 0 then NPSNoActiveNodes else NPSAvgLat $ a / fromIntegral b) <$> sumLatD <*> activeND
  valueOptionDyn $ NPSActiveNum <$> activeND
  descrOption $ NPSNodesNum $ length nodes
  descrOptionDyn avgLatD
  labelHorSep

-- TODO: use dyn settings instead of simple settings <- getSettings
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
              difC = curs \\ (_pubStorage'activeCurrencies ps)
              mpath = auth ^. walletInfo'storage . storage'pubStorage . pubStorage'pathPrefix
              mL = Map.fromList [(currency, mkStore mpath prvStr currency) | currency <- difC ]
              authN2 = authNew & walletInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages %~ (Map.union mL)
          pure $ Just $ authN2
      setWalletInfoE <- setWalletInfo updateAE
      void $ storeWallet "currenciesPage" (void $ updated authD)
      showSuccessMsg $ STPSSuccess <$ setWalletInfoE
      pure ()
  where
    uac cE =  updateActiveCurs $ fmap (\cl -> const (S.fromList cl)) $ cE
    mkStore mpath prvStr currency = createCurrencyPubStorage mpath (_prvStorage'rootPrvKey prvStr) (filterStartingHeight currency) currency

data FiatSelection = NoFiat | YesFiat
  deriving (Eq)

instance LocalizedPrint FiatSelection where
  localizedShow l v = case l of
    English -> case v of
      NoFiat -> "Hide fiat balance"
      YesFiat -> "Show balance in fiat"
    Russian -> case v of
      NoFiat -> "Не отображать фиатный баланс"
      YesFiat -> "Фиатный баланс в"

data RateSelection = NoRate | YesRate
  deriving (Eq)

instance LocalizedPrint RateSelection where
  localizedShow l v = case l of
    English -> case v of
      NoRate -> "Hide fiat rate"
      YesRate -> "Show rate for"
    Russian -> case v of
      NoRate -> "Не отображать курс"
      YesRate -> "Показывать курс к"

-- TODO: uncomment commented lines when ERGO is ready
unitsPage :: MonadFront t m => m ()
unitsPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure unitsPage) $ void $ workflow content
  where
    content = Workflow $ do
      h4 $ localizedText $ STPSSelectUnitsFor BTC
      nextE <- divClass "initial-options grid1" $ do
        setUnitE <- unitsSelectionWidget
        labelHorSep
        setFiatE <- fiatSelectionWidget
        labelHorSep
        setRateE <- rateSelectionWidget
        delay 0.1 $ leftmost [setUnitE, setFiatE, setRateE]
      pure ((), content <$ nextE)

    unitsSelectionWidget :: MonadFront t m => m (Event t ())
    unitsSelectionWidget = do
      settings <- getSettings
      let setUs = fromMaybe defUnits . settingsUnits $ settings
      unitBtcE <- unitsDropdown (getUnitBTC setUs) allUnitsBTC
      updateSettings $ ffor unitBtcE (\ubtc -> settings {settingsUnits = Just $ setUs {unitBTC = Just ubtc}})

    fiatSelectionWidget :: MonadFront t m => m (Event t ())
    fiatSelectionWidget = do
      settings <- getSettings
      let initSel = maybe NoFiat (const YesFiat) $ settingsFiatCurr settings
          initFiat = fromMaybe USD $ settingsFiatCurr settings
      selE <- divClass "navbar-2-cols mb-2" $ do
        noFiatE <- navbarBtn NoFiat initSel
        fiatE <- navbarBtn YesFiat initSel
        pure $ leftmost [noFiatE, fiatE]
      selD <- holdDyn initSel selE
      symbE <- networkHoldDynE $ ffor selD $ \case
        NoFiat -> pure never
        YesFiat -> unitsDropdown initFiat allFiats
      let detSymbE = ffor selE $ \case
            NoFiat -> Nothing
            YesFiat -> Just initFiat
      let setE = leftmost [Just <$> symbE, detSymbE]
      updateSettings $ ffor setE $ \ms -> settings {settingsFiatCurr = ms}

    rateSelectionWidget :: MonadFront t m => m (Event t ())
    rateSelectionWidget = do
      settings <- getSettings
      let initSel = maybe NoRate (const YesRate) $ settingsRateFiat settings
          initFiat = fromMaybe USD $ settingsRateFiat settings
      selE <- divClass "navbar-2-cols mb-2" $ do
        noFiatE <- navbarBtn NoRate initSel
        fiatE <- navbarBtn YesRate initSel
        pure $ leftmost [noFiatE, fiatE]
      selD <- holdDyn initSel selE
      symbE <- networkHoldDynE $ ffor selD $ \case
        NoRate -> pure never
        YesRate -> unitsDropdown initFiat allFiats
      let detSymbE = ffor selE $ \case
            NoRate -> Nothing
            YesRate -> Just initFiat
      let setE = leftmost [Just <$> symbE, detSymbE]
      updateSettings $ ffor setE $ \ms -> settings {settingsRateFiat = ms}

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

    navbarBtn :: (DomBuilder t m, PostBuild t m, MonadLocalized t m, LocalizedPrint l, Eq l)
      => l -> l -> m (Event t l)
    navbarBtn item activeItem
      | item == activeItem = spanButton "navbar-item active" item >> pure never
      | item /= activeItem = (item <$) <$> spanButton "navbar-item" item
    navbarBtn _ _ = pure never

portfolioPage :: MonadFront t m => m ()
portfolioPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure portfolioPage) $ do
    h3 $ localizedText STPSSetsPortfolio
    divClass "initial-options" $ mdo
      settings <- getSettings
      let sFC = fromMaybe USD $ settingsFiatCurr settings
      divClass "select-currencies-title" $ h4 $ localizedText STPSSetsPortfolioEnable
      portD <- holdDyn (settingsPortfolio settings) $ poke pbtnE $ \_ -> do
        portS <- sampleDyn portD
        pure $ not portS
      pbtnE <- divButton (fmap toggled portD) $ networkHoldDyn $ ffor portD $ \pS ->
        if pS
          then localizedText CSOn
          else localizedText CSOff
      void $ updateSettings $ ffor (updated portD) (\portS -> settings {settingsPortfolio = portS})
      divClass "select-currencies-title" $ h4 $ localizedText STPSSetsFiatSelect
      fiatE <- fiatDropdown sFC allFiats
      void $ updateSettings $ ffor fiatE (\fiat -> settings {settingsFiatCurr = Just $ fiat})
  where
    toggled b = if b
      then "button button-on button-currency"
      else "button button-off button-currency"

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
      nextE <- withWallet $ (const $ pure ()) <$ reqPassE
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

rbfPage :: MonadFront t m => m ()
rbfPage = do
  title <- localized STPSButRbf
  wrapper True title (Just $ pure rbfPage) $ do
    settings <- getSettings
    let initVal = btcSettings'sendRbfByDefault $ getBtcSettings settings
    initValD <- holdDyn initVal never
    valD <- toggler STPSEnableRbfByDefault initValD
    selE <- fmap updated $ holdUniqDyn valD
    updE <- updateSettings $ ffor selE (\rbfSetting -> setRbfSetting settings rbfSetting)
    showSuccessMsg $ STPSSuccess <$ updE
    pure ()
  where
    setRbfSetting :: Settings -> Bool -> Settings
    setRbfSetting s val =
      let oldSettings = settingsCurrencySpecific s
          oldBtcSettings = getBtcSettings s
      in s {settingsCurrencySpecific = Map.insert BTC (SettingsBtc $ oldBtcSettings {btcSettings'sendRbfByDefault = val}) oldSettings}

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
