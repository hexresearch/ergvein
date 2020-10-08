module Ergvein.Wallet.Page.Settings.Unauth
  (
    settingsPageUnauth
  , languagePageWidget
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

dnsPageWidget :: MonadFrontBase t m => m ()
dnsPageWidget = do
  h3 $ localizedText STPSButDns
  setsD <- getSettingsD
  actD <- widgetHoldDyn $ ffor setsD $ \Settings{..} -> divClass "p-1 fit-content ml-a mr-a" $
    flip traverse (S.toList settingsDns) $ \url -> mdo
      tglD <- toggle False editE
      (delE, editE) <- divClass "network-name mt-1 pl-2" $ do
        divClass "mt-a mb-a network-name-txt" $ text $ T.pack url
        delE <- buttonClass "button button-outline network-edit-btn mt-a mb-a ml-2" ("Delete" :: Text)
        editE <- buttonClassDynLabel "button button-outline mt-a mb-a ml-1" $ ffor tglD $ \case
          False -> NSSEdit
          True -> NSSCancel
        pure (delE, editE)
      setE <- addDnsWidget url tglD
      widgetHoldDyn $ ffor tglD $ \case
        True -> elClass "hr" "network-hr-sep-lb m-0 mt-1" $ pure ()
        False -> pure ()
        
      pure $ leftmost [Right . (url,) <$> setE, Left url <$ delE]
  let actE = switchDyn $ leftmost <$> actD
  modifySettings $ ffor actE $ \act s -> case act of
    Left u -> s {settingsDns = S.delete u (settingsDns s)}
    Right (u,u') -> let us' = S.insert u' $ S.delete u (settingsDns s)
      in s {settingsDns = us'}
  pure ()


addDnsWidget :: forall t m . MonadFrontBase t m => HostName -> Dynamic t Bool -> m (Event t HostName)
addDnsWidget initIp showD = fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b -> if not b then pure never else do
  murlE <- divClass "mt-3" $ do
    textD <- fmap _inputElement_value $ inputElement $ def
      & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
    goE <- outlineButton NSSAddDns
    pure $ poke goE $ const $ do
      t <- sampleDyn textD
      let val :: [Maybe Word8] = fmap (readMaybe . T.unpack) $ T.splitOn "." t
      pure $ case val of
        (Just a):(Just b):(Just c):(Just d):[] -> Just $ T.unpack t
        _ -> Nothing
  void $ widgetHold (pure ()) $ ffor murlE $ \case
    Nothing -> divClass "form-field-errors" $ localizedText NSSFailedDns
    _ -> pure ()
  pure $ fmapMaybe id murlE

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
