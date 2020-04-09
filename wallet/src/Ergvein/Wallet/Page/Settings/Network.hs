{-# LANGUAGE OverloadedLists #-}
-- {-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Settings.Network
  (
    networkSettingsPage
  ) where

import Data.Maybe (isJust, fromJust)
import Data.Time
import Reflex.Dom
import Servant.Client(BaseUrl, showBaseUrl)
import Data.Bifunctor
import Text.Read

import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Input
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings

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

networkSettingsPage :: MonadFront t m => m ()
networkSettingsPage = divClass "base-container" $ do
  headerWidget NSSTitle $ Just $ pure networkSettingsPage
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
        maybe (Left ["Failed to parse NominalDiffTime" :: Text]) Right . readMaybe . T.unpack
      actNumD <- textFieldValidated NSSActUrlNum settingsActUrlNum $
        maybe (Left ["Failed to parse NominalDiffTime" :: Text]) Right . readMaybe . T.unpack
      rminD <- textFieldValidated NSSReqNumMin (fst settingsReqUrlNum) $
        maybe (Left ["Failed to parse NominalDiffTime" :: Text]) Right . readMaybe . T.unpack
      rmaxD <- textFieldValidated NSSReqNumMax (snd settingsReqUrlNum) $
        maybe (Left ["Failed to parse NominalDiffTime" :: Text]) Right . readMaybe . T.unpack
      pure $ (,,,) <$> dtD <*> actNumD <*> rminD <*> rmaxD
  pure ()
  where
    fmap2 = fmap . fmap

activePageWidget :: MonadFront t m => m ()
activePageWidget = do
  lineOption $ do
    refreshIndexerInfo =<< buttonClass "button button-outline mt-1" NSSRefresh
    void $ buttonClass "button button-outline mt-1" NSSRestoreUrls
  divClass "centered-wrapper" $ divClass "centered-content" $ mdo
    allIndsD <- (fmap . fmap) (M.mapKeys Just) getIndexerInfoD
    keyD <- foldDyn (\nk ok -> if nk == ok then Nothing else nk) Nothing selE
    selE <- selectViewListWithKey_ keyD allIndsD $ \murl minfoD bD -> lineOption $
      fmap (switch . current) $ widgetHoldDyn $ ffor bD $ \b -> do
        let url = fromJust murl
        e <- divE $ widgetHoldDyn $ ffor minfoD $ \minfo -> do
          divClass "network-name" $ do
            let cls = if isJust minfo then "indexer-online" else "indexer-offline"
            elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
            text $ T.pack . showBaseUrl $ url
          descrOption $ maybe NSSOffline (NSSLatency . indInfoLatency) minfo
        widgetHoldDyn $ (\b -> if b then editWidget url else pure ()) <$> bD
        labelHorSep
        pure e
    pure ()
  where
    editWidget :: MonadFront t m => BaseUrl -> m ()
    editWidget url = lineOption $ do
      deactivateURL . (url <$) =<< outlineButton NSSDisable
      forgetURL . (url <$) =<< outlineButton NSSForget
      pure ()

inactivePageWidget :: forall t m . MonadFront t m => m ()
inactivePageWidget = do
  urlsD <- (fmap . fmap) S.toList getInactiveUrlsD
  pingAllE <- lineOption $ buttonClass "button button-outline mt-1" NSSPingAll
  allResE <- fmap switchDyn $ widgetHoldDyn $ ffor urlsD $ \urls ->
    fmap (mergeMap . M.fromList) $ flip traverse urls $ \u -> do
      resE <- pingIndexer $ u <$ pingAllE
      pure (u, snd <$> resE)
  divClass "centered-wrapper" $ divClass "centered-content" $ mdo
    keyD <- foldDyn (\nk ok -> if nk == ok then Nothing else nk) Nothing $ Just <$> selE
    infomapD <- foldDyn M.union M.empty $ leftmost [resE, allResE]
    selD <- simpleList urlsD $ \urlD -> do
      let isMeD = (\u murl -> Just u == murl) <$> urlD <*> keyD
          myInfoD = M.lookup <$> urlD <*> infomapD
          allD = (,,) <$> urlD <*> isMeD <*> myInfoD
      evntsD <- widgetHoldDyn $ renderItem <$> allD
      pure $ bimap switchDyn switchDyn $ splitDynPure evntsD
    let (selE, pingE) = bimap switchDyn switchDyn . splitDynPure . fmap (bimap leftmost leftmost . unzip) $ selD
    resE <- (fmap . fmap) (uncurry M.singleton) $ pingIndexer pingE
    pure ()
  where
    renderItem :: MonadFront t m => (BaseUrl, Bool, Maybe (Maybe IndexerInfo)) -> m (Event t BaseUrl, Event t BaseUrl)
    renderItem (url, me, mminfo) = do
      e <- divE $ case mminfo of
        Nothing -> divClass "network-name" $ text $ T.pack . showBaseUrl $ url
        Just minfo -> do
          divClass "network-name" $ do
            let cls = if isJust minfo then "indexer-online" else "indexer-offline"
            elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
            text $ T.pack . showBaseUrl $ url
          descrOption $ maybe NSSOffline (NSSLatency . indInfoLatency) minfo
      pingE <- if not me then (pure never) else lineOption $ do
        activateURL . (url <$) =<< outlineButton NSSEnable
        pingE <- outlineButton NSSPing
        forgetURL . (url <$) =<< outlineButton NSSForget
        pure pingE
      labelHorSep
      pure $ (url <$ e, url <$ pingE)

navbarWidget :: MonadFront t m => NavbarItem -> m (Dynamic t NavbarItem)
navbarWidget initItem = divClass "navbar" $ mdo
  selD <- holdDyn initItem selE
  selE <- fmap leftmost $ flip traverse [ActivePage, DisabledPage, ParametersPage] $ \i -> do
    let attrD = (\ai -> "navbar-item" <> if i == ai then " active" else "") <$> selD
    pure . (<$) i =<< spanButton attrD i
  pure selD

lineOption :: MonadFront t m => m a -> m a
lineOption = divClass "network-wrapper" . divClass "network-line"

lineOptionE :: MonadFront t m => m a -> m (Event t ())
lineOptionE ma = do
  (e,_) <- elAttr' "div" ("class" =: "network-wrapper") $ divClass "network-line" ma
  pure $ void $ domEvent Click e

divE :: MonadFront t m => m a -> m (Event t ())
divE ma = fmap (domEvent Click . fst) $ el' "div" ma

nameOption, descrOption :: (MonadFront t m, LocalizedPrint a) => a -> m ()
nameOption = divClass "network-name"    . localizedText
descrOption = (>>) elBR . divClass "network-descr" . localizedText

valueOptionDyn, descrOptionDyn :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
descrOptionDyn v = getLanguage >>= \langD -> (>>) elBR (divClass "network-descr" $ dynText $ ffor2 langD v localizedShow)

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-lb")] blank
elBR = el "br" blank
