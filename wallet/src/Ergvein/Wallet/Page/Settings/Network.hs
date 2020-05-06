{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Settings.Network
  (
    networkSettingsPage
  ) where

import Control.Lens
import Data.Bifunctor
import Data.Maybe (isJust, fromJust)
import Data.Time
import Reflex.Dom
import Servant.Client(BaseUrl, showBaseUrl, parseBaseUrl)
import Text.Read

import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Clipboard
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Set as S

data NavbarItem = ActivePage | DisabledPage | ParametersPage
  deriving (Eq)

instance LocalizedPrint NavbarItem where
  localizedShow l v = case l of
    English -> case v of
      ActivePage      -> "Active indexers"
      DisabledPage    -> "Reserved indexers"
      ParametersPage  -> "Network parameters"
    Russian -> case v of
      ActivePage      -> "Используемые индексеры"
      DisabledPage    -> "Запасные индексеры"
      ParametersPage  -> "Сетевые параметры"

data ParametersParseErrors = PPENDT | PPEInt

instance LocalizedPrint ParametersParseErrors where
  localizedShow l v = case l of
    English -> case v of
      PPENDT -> "Failed to parse seconds"
      PPEInt -> "Failed to parse integer"
    Russian -> case v of
      PPENDT -> "Некорректное значение. Только дробные числа"
      PPEInt -> "Некорректное значение. Только целые числа"

networkSettingsPage :: MonadFront t m => m ()
networkSettingsPage = wrapper NSSTitle (Just $ pure networkSettingsPage ) False $ do
  navD <- navbarWidget ActivePage
  void $ widgetHoldDyn $ ffor navD $ \case
    ActivePage      -> activePageWidget
    DisabledPage    -> inactivePageWidget
    ParametersPage  -> parametersPageWidget

parametersPageWidget :: MonadFront t m => m ()
parametersPageWidget = mdo
  setD <- getSettingsD
  lineOption $ do
    saveE <- buttonClass "button button-outline mt-1" NSSSave
    defE <- buttonClass "button button-outline mt-1" NSSRestoreDef
    updE <- updateSettings $ flip pushAlways defE $ const $ do
      set <- sample $ current setD
      pure $ set {
            settingsReqTimeout = defaultIndexerTimeout
          , settingsReqUrlNum  = defaultIndexersNum
          , settingsActUrlNum  = defaultActUrlNum
        }
    updE' <- updateSettings $ flip pushAlways saveE $ const $ do
      set <- sample $ current setD
      (dt, actNum, rmin, rmax) <- sample $ current valsD
      pure $ set {
            settingsReqTimeout = dt
          , settingsReqUrlNum  = (rmin, rmax)
          , settingsActUrlNum  = actNum
        }
    showSuccessMsg $ STPSSuccess <$ (leftmost [updE, updE'])
  valsD <- fmap join $ divClass "centered-wrapper" $ divClass "centered-content" $
    widgetHoldDyn $ ffor setD $ \set@Settings{..} -> do
      let dt0 :: Double = realToFrac settingsReqTimeout
      dtD <- fmap2 realToFrac $ textFieldValidated NSSReqTimeout dt0 $
        maybe (Left [PPENDT]) Right . readMaybe . T.unpack
      actNumD <- textFieldValidated NSSActUrlNum settingsActUrlNum $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      rminD <- textFieldValidated NSSReqNumMin (fst settingsReqUrlNum) $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      rmaxD <- textFieldValidated NSSReqNumMax (snd settingsReqUrlNum) $
        maybe (Left [PPEInt]) Right . readMaybe . T.unpack
      pure $ (,,,) <$> dtD <*> actNumD <*> rminD <*> rmaxD
  pure ()
  where
    fmap2 = fmap . fmap

addUrlWidget :: forall t m . MonadFront t m => Dynamic t Bool -> m (Event t BaseUrl)
addUrlWidget showD = mdo
  hideE <- fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b -> if not b then pure never else do
    murlE <- lineOption $ do
      textD <- fmap _inputElement_value $ inputElement $ def
        & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ ("type" =: "text")
      goE <- outlineButton NSSAddUrl
      let urlE :: Event t (Maybe BaseUrl) = fmap (parseBaseUrl . T.unpack) $ current textD `tag` goE
      pure urlE
    widgetHold (pure ()) $ ffor murlE $ \case
      Nothing -> divClass "form-field-errors" $ text "Falied to parse URL"
      _ -> pure ()
    pure $ fmapMaybe id murlE
  pure hideE

activePageWidget :: forall t m . MonadFront t m => m ()
activePageWidget = mdo
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  tglE <- divClass "network-wrapper mt-1" $ divClass "net-header-btns-3" $ do
    refreshIndexerInfo =<< buttonClass "button button-outline m-0" NSSRefresh
    restoreDefaultIndexers =<< buttonClass "button button-outline m-0" NSSRestoreUrls
    fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "button button-outline m-0" $ if b then NSSClose else NSSAddUrl
  hideE <- activateURL =<< addUrlWidget showD
  divClass "centered-wrapper" $ divClass "lr-centered-content" $
    void . flip listWithKey renderActive =<< getIndexerInfoD

renderActive :: MonadFront t m => BaseUrl -> Dynamic t (Maybe IndexerInfo) -> m ()
renderActive url minfoD = mdo
  tglD <- holdDyn False tglE
  tglE <- fmap switchDyn $ widgetHoldDyn $ ffor minfoD $ \minfo -> lineOption $ do
    tglE' <- divClass "network-name" $ do
      let cls = if isJust minfo then "mt-a mb-a indexer-online" else "mt-a mb-a indexer-offline"
      elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
      divClass "mt-a mb-a network-name-txt" $ text $ T.pack . showBaseUrl $ url
      fmap switchDyn $ widgetHoldDyn $ ffor tglD $ \b ->
        fmap (not b <$) $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a" $ if b then NSSClose else NSSEdit
    descrOption $ maybe NSSOffline (NSSLatency . indInfoLatency) minfo
    pure tglE'
  widgetHoldDyn $ ffor tglD $ \b -> if not b
    then pure ()
    else divClass "network-wrapper mt-2" . divClass "network-line" $ do
      deactivateURL . (url <$) =<< buttonClass "button button-outline mt-1 ml-1" NSSDisable
      forgetURL . (url <$) =<< buttonClass "button button-outline mt-1 ml-1" NSSForget
      pure ()
  pure ()

inactivePageWidget :: forall t m . MonadFront t m => m ()
inactivePageWidget = mdo
  urlsD <- (fmap . fmap) S.toList getInactiveUrlsD
  showD <- holdDyn False $ leftmost [False <$ hideE, tglE]
  (pingAllE, tglE) <- divClass "network-wrapper mt-1" $ divClass "net-header-btns-2" $ do
    pingAllE <- buttonClass "button button-outline m-0" NSSPingAll
    tglE <- fmap switchDyn $ widgetHoldDyn $ ffor showD $ \b ->
      fmap (not b <$) $ buttonClass "m-0 button button-outline" $ if b then NSSClose else NSSAddUrl
    pure (pingAllE, tglE)
  hideE <- deactivateURL =<< addUrlWidget showD
  allResE <- fmap switchDyn $ widgetHoldDyn $ ffor urlsD $ \urls ->
    fmap (mergeMap . M.fromList) $ flip traverse urls $ \u -> do
      resE <- pingIndexer $ u <$ pingAllE
      pure (u, snd <$> resE)
  divClass "centered-wrapper" $ divClass "lr-centered-content" $ mdo
    infomapD <- foldDyn M.union M.empty $ leftmost [resE, allResE]
    a :: Dynamic t [Dynamic t (Event t BaseUrl)] <- simpleList urlsD $ \urlD -> do
      let myInfoD = M.lookup <$> urlD <*> infomapD
      widgetHoldDyn $ renderInactive <$> urlD <*> myInfoD
    let pingE = switchDyn . fmap leftmost . join . fmap sequence $ a
    resE <- (fmap . fmap) (uncurry M.singleton) $ pingIndexer pingE
    pure ()

renderInactive :: MonadFront t m => BaseUrl -> Maybe (Maybe IndexerInfo) -> m (Event t BaseUrl)
renderInactive url mminfo = mdo
  let urlTxt = T.pack $ showBaseUrl url
  tglD <- holdDyn False tglE
  tglE <- lineOption $ do
    tglE' <- divClass "network-name" $ do
      case mminfo of
        Nothing -> divClass "mt-a mb-a network-name-txt" $ text urlTxt
        Just minfo -> do
          let cls = if isJust minfo then "mt-a mb-a indexer-online" else "mt-a mb-a indexer-offline"
          elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
          divClass "mt-a mb-a network-name-txt" $ text urlTxt
      fmap switchDyn $ widgetHoldDyn $ ffor tglD $ \b ->
        fmap (not b <$) $ buttonClass "button button-outline network-edit-btn mt-a mb-a ml-a" $ if b then NSSClose else NSSEdit
    maybe (pure ()) (descrOption . maybe NSSOffline (NSSLatency . indInfoLatency)) mminfo
    pure tglE'
  pingE <- fmap switchDyn $ widgetHoldDyn $ ffor tglD $ \b -> if not b
    then pure never
    else divClass "network-wrapper mt-2" . divClass "network-line" $ do
      activateURL . (url <$) =<< outlineButton NSSEnable
      pingE <- fmap (url <$) $ outlineButton NSSPing
      forgetURL . (url <$) =<< outlineButton NSSForget
      pure pingE
  pure pingE

navbarWidget :: MonadFront t m => NavbarItem -> m (Dynamic t NavbarItem)
navbarWidget initItem = divClass "navbar" $ mdo
  selD <- holdDyn initItem selE
  selE <- fmap leftmost $ flip traverse [ActivePage, DisabledPage, ParametersPage] $ \i -> do
    let attrD = (\ai -> "navbar-item" <> if i == ai then " active" else "") <$> selD
    pure . (<$) i =<< spanButton attrD i
  pure selD

lineOption :: MonadFront t m => m a -> m a
lineOption = divClass "network-wrapper mt-1" . divClass "network-line"

lineOptionE :: MonadFront t m => m a -> m (Event t ())
lineOptionE ma = do
  (e,_) <- elAttr' "div" ("class" =: "network-wrapper") $ divClass "network-line" ma
  pure $ void $ domEvent Click e

divE :: MonadFront t m => m a -> m (Event t ())
divE ma = fmap (domEvent Click . fst) $ el' "div" ma

nameOption, descrOption :: (MonadFront t m, LocalizedPrint a) => a -> m ()
nameOption = divClass "network-name"    . localizedText
descrOption = divClass "network-descr" . localizedText

valueOptionDyn, descrOptionDyn :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
descrOptionDyn v = getLanguage >>= \langD -> (>>) elBR (divClass "network-descr" $ dynText $ ffor2 langD v localizedShow)

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-nomargin")] blank
elBR = el "br" blank
