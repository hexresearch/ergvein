module Ergvein.Wallet.Page.Settings.Unauth
  (
    settingsPageUnauth
  , languagePageWidget
  , dnsPageWidget
  ) where

import Control.Monad
import Data.Text
import Data.Word
import Network.Socket
import Reflex.Dom
import Reflex.Dom.Retractable
import Text.Read

import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Page.Settings.Network
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Inplace

import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T

-- TODO: uncomment commented lines when ERGO is ready
data SubPageSettings
  = GoLanguage
  | GoNetwork
  | GoDns

settingsPageUnauth :: MonadFrontBase t m => m ()
settingsPageUnauth = wrapperSimple True $ do
  divClass "initial-options grid1" $ do
    goLangE            <- fmap (GoLanguage   <$) $ outlineButton STPSButLanguage
    goNetE             <- fmap (GoNetwork    <$) $ outlineButton STPSButNetwork
    goDnsE             <- fmap (GoDns    <$) $ outlineButton STPSButDns
    let goE = leftmost [goLangE, goNetE, goDnsE]
    void $ nextWidget $ ffor goE $ \spg -> Retractable {
        retractableNext = case spg of
          GoLanguage  -> languagePageUnauth
          GoNetwork   -> networkSettingsPageUnauth
          GoDns       -> dnsPageUnauth
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
    editD <- widgetHoldDyn $ ffor setsD $ \s -> do
      when (S.null $ settingsDns s) $ h4 $ localizedText NSSResolveConfDefault
      traverse dnsWidget $ S.toList $ settingsDns s
    addE <- addDnsWidget
    restoreE <- buttonClass "button button-outline ml-a mr-a w-100" NSSRestoreUrls
    let editE = switchDyn $ leftmost <$> editD
    pure $ leftmost [addE, editE, DnsRestore <$ restoreE]
  modifySettings $ ffor actE $ \act s -> case act of
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
      valD <- widgetHoldDyn $ ffor tglD $ \case
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

dnsWidget :: MonadFrontBase t m => HostName -> m (Event t DnsAction)
dnsWidget url = divClass "network-name mt-1 pl-2" $ do
  let cfg = InplaceEditCfg {
         _inplaceCanDelete   = pure True
       , _inplaceShowClass   = "mt-a mb-a network-name-txt ml-a"
       , _inplaceSeparator   = Just <$> "network-hr-sep-lb m-0 mt-1"
       , _inplaceEditLabel   = NSSEdit
       , _inplaceEditClass   = "button button-outline mt-a mb-a ml-1 mr-a"
       , _inplaceBtnGroup    = ""
       , _inplaceErrorClass  = "form-field-errors ta-c-imp"
       , _inplaceSaveLabel   = NSSSave
       , _inplaceDeleteLabel = NSSDelete
       , _inplaceCancelLabel = NSSCancel
      }
      parse t = maybe (Left NSSFailedDns) (const $ Right $ T.unpack t) . parseIP $ t
  actE <- inplaceEditField cfg T.pack parse (pure url)
  pure $ ffor actE $ \case
    EditDelete a -> DnsDel a
    EditUpdate a1 a2 -> DnsUpd a1 a2


-- | Validate ip and show an error if something is not ok
validateDNSIp :: MonadFrontBase t m => Event t Text -> m (Event t HostName)
validateDNSIp txtE = do
  void $ widgetHold (pure ()) $ ffor murlE $ \case
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
    void $ widgetHold (pure ()) $ setLanguage <$> selE
    settings <- getSettings
    updE <- updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
    showSuccessMsg $ STPSSuccess <$ updE
  pure ()
