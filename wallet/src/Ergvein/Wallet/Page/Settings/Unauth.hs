{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Settings.Unauth
  (
    settingsPageUnauth
  , languagePageWidget
  , dnsPageWidget
  , torPageWidget
  ) where

import Control.Monad
import Data.Bifunctor (bimap)
import Data.Text
import Network.Socket

import Ergvein.Wallet.IP
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Orphanage ()
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Wrapper
import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Validate

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

data SubPageSettings
  = GoLanguage
  | GoNetwork
  | GoDns
  | GoTor

settingsPageUnauth :: MonadFrontBase t m => m ()
settingsPageUnauth = wrapperSimple True $ do
  divClass "initial-options grid1" $ do
    goLangE <- (GoLanguage <$) <$> outlineButton STPSButLanguage
    goNetE  <- (GoNetwork <$)  <$> outlineButton STPSButNetwork
    goDnsE  <- (GoDns <$)      <$> outlineButton STPSButDns
    goTorE  <- (GoTor <$)      <$> outlineButton STPSButTor
    let goE = leftmost [goLangE, goNetE, goDnsE, goTorE]
    void $ nextWidget $ ffor goE $ \spg -> Retractable {
        retractableNext = case spg of
          GoLanguage -> languagePageUnauth
          GoNetwork  -> activeIndexersPageUnauth
          GoDns      -> dnsPageUnauth
          GoTor      -> torPageUnauth
      , retractablePrev = Just $ pure settingsPageUnauth
      }

dnsPageUnauth :: MonadFrontBase t m => m ()
dnsPageUnauth = wrapperSimple False dnsPageWidget

data DnsAction = DnsDel HostName | DnsUpd HostName HostName | DNSAdd HostName | DnsRestore

dnsPageWidget :: MonadFrontBase t m => m ()
dnsPageWidget = do
  h3 $ localizedText STPSButDns
  setsD <- getSettingsD
  actE <- divClass "p-1 fit-content ml-a mr-a" $ do
    editD <- networkHoldDyn $ ffor setsD $ \s -> do
      when (S.null $ settingsDns s) $ h4 $ localizedText NSSResolveConfDefault
      traverse dnsWidget $ S.toList $ settingsDns s
    addE <- addDnsWidget
    restoreE <- buttonClass "button button-outline ml-a mr-a w-100" NSSRestoreUrls
    let editE = switchDyn $ leftmost <$> editD
    pure $ leftmost [addE, editE, DnsRestore <$ restoreE]
  void $ modifySettings $ ffor actE $ \act s -> case act of
    DnsDel u -> s {settingsDns = S.delete u (settingsDns s)}
    DnsUpd u u' -> let us' = S.insert u' $ S.delete u (settingsDns s)
      in s {settingsDns = us'}
    DNSAdd u -> s {settingsDns = S.insert u $ settingsDns s}
    DnsRestore -> s {settingsDns = defaultDns}
  where
    addDnsWidget :: MonadFrontBase t m => m (Event t DnsAction)
    addDnsWidget = divClass "mt-3" $ mdo
      elClass "hr" "network-hr-sep-lb m-0 mt-1 mb-1" $ pure ()
      tglD <- toggle False tglE
      valD <- networkHoldDyn $ ffor tglD $ \case
        False -> (, never) <$> buttonClass "button button-outline ml-a mr-a w-100" NSSAddDns
        True -> do
          textD <- fmap _inputElement_value $ inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
          divClass "" $ do
            goE <- outlineButton NSSSave
            closeE <- outlineButton NSSCancel
            setE <- validateDNSIp $ current textD `tag` goE
            pure (closeE, DNSAdd <$> setE)
      let (tglE', actE) = bimap switchDyn switchDyn $ splitDynPure valD
      let tglE = leftmost [() <$ actE, tglE']
      pure actE

dnsWidget :: forall t m . MonadFrontBase t m => HostName -> m (Event t DnsAction)
dnsWidget url = divClass "network-name mt-1 pl-2" $ do
  let cfg = (def :: InplaceEditCfg t (InplaceEditLbl NetSetupStrings)) {
         _inplaceShowClass   = "mt-a mb-a network-name-txt ml-a"
       , _inplaceEditClass   = "button button-outline mt-a mb-a ml-1 mr-a"
      }
      parse t = maybe (Left $ InplaceError NSSFailedDns) (const $ Right $ T.unpack t) . parseIP $ t
  actE <- inplaceEditField cfg T.pack parse (pure url)
  pure $ ffor actE $ \case
    EditDelete a -> DnsDel a
    EditUpdate a1 a2 -> DnsUpd a1 a2

-- | Validate ip and show an error if something is not ok
validateDNSIp :: MonadFrontBase t m => Event t Text -> m (Event t HostName)
validateDNSIp txtE = do
  void $ networkHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors ta-c-imp" $ localizedText NSSFailedDns
    _ -> pure ()
  pure $ fmapMaybe id murlE
  where
    murlE = ffor txtE $ \t -> T.unpack t <$ parseIP t

languagePageUnauth :: MonadFrontBase t m => m ()
languagePageUnauth = wrapperSimple True languagePageWidget

-- | The same for both auth and unauth contexts
languagePageWidget :: MonadFrontBase t m => m ()
languagePageWidget = do
  h3 $ localizedText STPSSelectLanguage
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
    selE <- updated <$> holdUniqDyn selD
    void $ networkHold (pure ()) $ setLanguage <$> selE
    settings <- getSettings
    updE <- updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
    showSuccessMsg $ STPSSuccess <$ updE
  pure ()

torPageUnauth :: MonadFrontBase t m => m ()
torPageUnauth = wrapperSimple True torPageWidget

data ConnectAction = Enable | Disable deriving (Eq, Show)

torPageWidget :: MonadFrontBase t m => m ()
torPageWidget = mdo
  mConf <- getProxyConf
  divClass "mb-2" $ do
    h4 $ localizedText STPSTorStatus
    void $ networkHoldDyn $ ffor mConf $ \case
      Just (SocksConf addr port) -> do
        localizedText STPSTorEnabled
        text $ " " <> showt addr <> ":" <> showt port
      Nothing -> localizedText STPSTorDisabled
  h4 $ localizedText STPSSetsProxy
  divClass "mb-2" $ divClass "initial-options grid1" $ socksSettings connE
  connE <- networkHoldDynE $ ffor mConf $ \case
    Just _ -> (Disable <$) <$> submitClass "button button-outline" STPSTorDisable
    Nothing -> (Enable <$) <$> submitClass "button button-outline" STPSTorEnable
  pure ()
  where
    socksSettings connectionE = void $ mdo
      let connectE = fmapMaybe (\action -> if action == Enable then Just () else Nothing) connectionE
          disconnectE = fmapMaybe (\action -> if action == Disable then Just () else Nothing) connectionE
      msocksD <- getProxyConf
      initAddr <- sampleDyn $ maybe (showt $ socksConfAddr torSocks) (showt . socksConfAddr) <$> msocksD
      initPort <- sampleDyn $ maybe (showt $ socksConfPort torSocks) (showt . socksConfPort) <$> msocksD
      addrErrsD <- mkErrsDyn connectE addrD (pure . toEither . (validate :: Text -> Validation [ValidationError] IP))
      portErrsD <- mkErrsDyn connectE portD (pure . toEither . validateInt)
      addrD <- divClass "mb-1" $ labeledTextField STPSProxyIpField (def & textInputConfig_initialValue .~ initAddr) addrErrsD 
      portD <- labeledTextField STPSProxyPortField (def & textInputConfig_initialValue .~ initPort) portErrsD
      let
        activateE = flip push connectE $ const $ do
          addrText <- sampleDyn addrD
          portText <- sampleDyn portD
          let mAddr = eitherToMaybe $ toEither $ validate addrText
              mPort = eitherToMaybe $ toEither $ validateInt portText
          case (mAddr, mPort) of
            (Just addr, Just port) -> do
              pure $ Just (addr, port)
            _ -> pure Nothing
      void $ modifySettings $ ffor activateE $ \(addr, port) setts -> setts {
          settingsSocksProxy = Just $ SocksConf addr port
        }
      void $ modifySettings $ ffor disconnectE $ \_ setts -> setts {
          settingsSocksProxy = Nothing
        }