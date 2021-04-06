module Ergvein.Wallet.Page.Settings.Unauth
  (
    settingsPageUnauth
  , languagePageWidget
  , dnsPageWidget
  , torPageWidget
  ) where

import Control.Monad
import Data.Maybe
import Data.Text
import Network.Socket
import Reflex.Dom
import Reflex.Dom.Retractable

import Sepulcas.Alert
import Sepulcas.Elements
import Sepulcas.Elements.Inplace
import Sepulcas.Elements.Input
import Sepulcas.Elements.Toggle
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Inplace
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T

-- TODO: uncomment commented lines when ERGO is ready
data SubPageSettings
  = GoLanguage
  | GoNetwork
  | GoDns
  | GoTor

settingsPageUnauth :: MonadFrontBase t m => m ()
settingsPageUnauth = wrapperSimple True $ do
  divClass "initial-options grid1" $ do
    goLangE            <- fmap (GoLanguage   <$) $ outlineButton STPSButLanguage
    goNetE             <- fmap (GoNetwork    <$) $ outlineButton STPSButNetwork
    goDnsE             <- fmap (GoDns        <$) $ outlineButton STPSButDns
    goTorE             <- fmap (GoTor        <$) $ outlineButton STPSButTor
    let goE = leftmost [goLangE, goNetE, goDnsE, goTorE]
    void $ nextWidget $ ffor goE $ \spg -> Retractable {
        retractableNext = case spg of
          GoLanguage  -> languagePageUnauth
          GoNetwork   -> networkSettingsPageUnauth
          GoDns       -> dnsPageUnauth
          GoTor       -> torPageUnauth
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
  pure ()
  where
    addDnsWidget :: MonadFrontBase t m => m (Event t DnsAction)
    addDnsWidget = divClass "mt-3" $ mdo
      elClass "hr" "network-hr-sep-lb m-0 mt-1 mb-1" $ pure ()
      tglD <- toggle False tglE
      valD <- networkHoldDyn $ ffor tglD $ \case
        False -> fmap (, never) $ buttonClass "button button-outline ml-a mr-a w-100" NSSAddDns
        True -> do
          textD <- fmap _inputElement_value $ inputElement $ def
            & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
          divClass "" $ do
            goE <- outlineButton NSSSave
            closeE <- outlineButton NSSCancel
            setE <- validateDNSIp $ current textD `tag` goE
            pure (closeE, DNSAdd <$> setE)
      let (tglE', actE) = (\(a,b) -> (switchDyn a, switchDyn b)) $ splitDynPure valD
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
    void $ networkHold (pure ()) $ setLanguage <$> selE
    settings <- getSettings
    updE <- updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
    showSuccessMsg $ STPSSuccess <$ updE
  pure ()

torPageUnauth :: MonadFrontBase t m => m ()
torPageUnauth = wrapperSimple True torPageWidget

-- | The same for both auth and unauth contexts
torPageWidget :: MonadFrontBase t m => m ()
torPageWidget = do
  h3 $ localizedText STPSSetsTor
  divClass "initial-options grid1" torToggleButton
  h3 $ localizedText STPSSetsProxy
  divClass "initial-options grid1" socksSettings
  where
    torToggleButton = void $ do
      torUsedD <- fmap (maybe False (torSocks ==)) <$> getProxyConf
      torD <- toggler STPSUseTor torUsedD
      let updateE = flip push (updated torD) $ \useTor -> do
            torUsed <- sample . current $ torUsedD
            pure $ if useTor == torUsed then Nothing else Just useTor
      modifySettings $ ffor updateE $ \useTor setts -> setts {
          settingsSocksProxy = if useTor then Just torSocks else Nothing
        }
    socksSettings = void $ do
      msocksD <- getProxyConf
      maddrD <- valueField STPSProxyIpField $ fmap socksConfAddr <$> msocksD
      mportD <- valueField STPSProxyPortField $ fmap socksConfPort <$> msocksD
      let newSocksE = ffor (updated $ (,) <$> maddrD <*> mportD) $ \(maddr, mport) -> case maddr of
            Nothing -> Nothing
            Just addr -> Just $ SocksConf addr (fromMaybe 9050 mport)
      modifySettings $ ffor newSocksE $ \socks setts -> setts {
          settingsSocksProxy = socks
        }
