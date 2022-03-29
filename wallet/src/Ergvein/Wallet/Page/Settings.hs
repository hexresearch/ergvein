{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Settings(
    settingsPage
  , torPage
  ) where

import Data.Traversable (for)
import Reflex.Dom

import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Settings.MnemonicExport
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Page.Settings.Unauth
import Ergvein.Wallet.Password
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Toggle

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data SubPageSettings
  = GoLanguage
  -- | GoCurrencies
  | GoUnits
  | GoNetwork
  | GoMnemonicExport
  | GoDns
  | GoTor
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
            -- , (GoCurrencies, STPSButActiveCurrs)
            , (GoDns, STPSButDns)
            , (GoTor, STPSButTor)
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
            -- GoCurrencies      -> currenciesPage
            GoNetwork         -> networkSettingsPage
            GoUnits           -> unitsPage
            GoMnemonicExport  -> mnemonicExportPage
            GoNodes           -> btcNodesPage
            GoRbf             -> rbfPage
            GoDns             -> dnsPage
            GoTor             -> torPage
            GoPassword        -> changePasswordPage
            GoDelete          -> deleteWalletPage
        , retractablePrev = Just $ pure settingsPage
        }

languagePage :: MonadFront t m => m ()
languagePage = do
  title <- localized STPSButLanguage
  wrapper True title (Just $ pure languagePage) languagePageWidget

dnsPage :: MonadFront t m => m ()
dnsPage = do
  title <- localized STPSButDns
  wrapper True title (Just $ pure dnsPage) dnsPageWidget

torPage :: MonadFront t m => m ()
torPage = do
  title <- localized STPSButTor
  wrapper True title (Just $ pure torPage) torPageWidget

unitsPage :: MonadFront t m => m ()
unitsPage = do
  title <- localized STPSButDisplay
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

btcNodesPage :: MonadFront t m => m ()
btcNodesPage = do
  icmd <- isCustomModeD
  title <- localizedDyn $ NDSSTitle <$> icmd
  wrapper False title (Just $ pure btcNodesPage) btcNodesWidget
  pure ()

btcNodesWidget :: MonadFront t m => m ()
btcNodesWidget = do
  btcNodesD <- getBtcNodesD
  void $ lineOption $ networkHoldDyn $ ffor btcNodesD $ \cm -> do
    let btcNodes = Map.elems cm
    btcNetworkWidget btcNodes
    mapM_ btcNodeWidget btcNodes
  labelHorSep
  icmd <- isCustomModeD
  void $ networkHoldDyn $ ffor icmd $ \b ->
    if b then btcCustomMode else btcPublicMode
  pure ()

btcCustomMode :: MonadFront t m => m ()
btcCustomMode = mdo
  tglD <- holdDyn False actE
  actE <- networkHoldDynE $ ffor tglD $ \case
    False -> divClass "" $ do
      goPub <- outlineButton NDSSSwitchPub
      void $ clearCustomNode =<< clearNodeConns goPub
      setupE <- outlineButton NDSSSetPriv
      pure $ True <$ setupE
    True -> do
      mnode <- sampleDyn =<< getCustomNodeD
      nodeD <- setupCustomNode mnode
      divClass "" $ do
        setE <- outlineButton NDSSSetNode
        void $ setCustomNode $ attachWithMaybe (\v _ -> v) (current nodeD) setE
        cancelE <- outlineButton NDSSCancel
        pure $ False <$ cancelE
  pure ()

btcPublicMode :: MonadFront t m => m ()
btcPublicMode = mdo
  tglD <- holdDyn False actE
  actE <- networkHoldDynE $ ffor tglD $ \case
    False -> divClass "" $ do
      setupE <- outlineButton NDSSSetPriv
      pure $ True <$ setupE
    True -> do
      mnode <- sampleDyn =<< getCustomNodeD
      nodeD <- setupCustomNode mnode
      divClass "" $ do
        setE <- outlineButton NDSSSetNode
        void $ setCustomNode $ attachWithMaybe (\v _ -> v) (current nodeD) setE
        cancelE <- outlineButton NDSSCancel
        pure $ False <$ cancelE
  pure ()

btcNodeWidget :: MonadFront t m => NodeBtc t -> m ()
btcNodeWidget node = do
  let offclass = [("class", "mt-a mb-a indexer-offline")]
  let onclass = [("class", "mt-a mb-a indexer-online")]
  let clsD = (\b -> if b then onclass else offclass) <$> nodeconIsUp node
  divClass "network-name" $ do
    let addr = nodeconUrl node
    unless (nodeconIsPrivate node) $ do
      (e,_) <- elAttr' "span" [("class", "mt-a mb-a mr-1")] $ elClass "i" "fas fa-times" $ pure ()
      let closeE = (addr, NodeMsgClose) <$ domEvent Click e
      void $ postNodeMessage BTC closeE
    elDynAttr "span" clsD $ elClass "i" "fas fa-circle" $ pure ()
    divClass "mt-a mb-a network-name-txt" $ text $ showt addr
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
      -- We need this to stop walletStoreThread
      storedE <- storeWalletNow "delete-wallet-page" True delE
      doneE <- performEvent $ ffor storedE $ const $ do
        deleteStoredFile walletName
        deleteStoredFile backupName
        deleteStoredFile ".last-wallet"
      void $ setWalletInfo $ Nothing <$ doneE
      pure ((), never)

lineOption :: MonadFront t m => m a -> m a
lineOption = divClass "network-wrapper"

descrOption :: (MonadFront t m, LocalizedPrint a) => a -> m ()
descrOption = (>>) elBR . divClass "network-descr" . localizedText

valueOptionDyn :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-lb")] blank
elBR = el "br" blank
