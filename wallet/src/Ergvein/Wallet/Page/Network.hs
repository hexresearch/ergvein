{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Network(
    networkPage
  ) where

import Data.Maybe (catMaybes, isJust)
import Reflex.ExternalRef

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Currency()
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Node
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

networkPage :: MonadFront t m => Maybe Currency -> m ()
networkPage curMb = do
  title <- localized NPSTitle
  wrapper False title (Just $ pure $ networkPage curMb) $ do
    valD <- networkPageHeader curMb
    void $ widgetHoldDyn $ ffor valD $ \case
      Nothing -> pure ()
      Just (cur, refrE) -> networkPageWidget cur refrE


networkPageWidget :: MonadFront t m => Currency -> Event t () -> m ()
networkPageWidget cur refrE = do
  lenlatD <- indexersAverageLatNumWidget refrE
  conmapD <- getNodeConnectionsD
  let (servNumD, avgLatD) = splitDynPure $ ffor lenlatD $ \(len, lat) -> let
        avgL = if len == 0 then NPSNoServerAvail else NPSAvgLat lat
        in (NPSServerVal len, avgL)
  listE <- lineOption $ do
    nameOption NPSServer
    listE <- el "div" $ do
      valueOptionDyn servNumD
      divClass "network-name-edit" $ fmap (cur <$) $ outlineButton NPSServerListView
    descrOption NPSServerDescr
    descrOptionDyn avgLatD
    labelHorSep
    pure listE
  -- lineOption $ lineOptionNoEdit NPSSyncStatus servCurInfoD NPSSyncDescr
  void $ lineOption $ widgetHoldDyn $ ffor conmapD $ \cm -> case cur of
    BTC  -> btcNetworkWidget $ maybe [] M.elems $ DM.lookup BTCTag cm
    ERGO -> ergNetworkWidget $ maybe [] M.elems $ DM.lookup ERGOTag cm
  void $ nextWidget $ ffor listE $ \с -> Retractable {
      retractableNext = serversInfoPage с
    , retractablePrev = Just (pure $ networkPage (Just с))
    }
  pure ()

btcNetworkWidget :: MonadFront t m => [NodeBTC t] -> m ()
btcNetworkWidget nodes = do
  infosD <- fmap sequence $ traverse externalRefDynamic $ nodeconStatus <$> nodes
  let activeND = fmap (length . filter id) $ sequence $ nodeconIsUp <$> nodes
      sumLatD  = fmap (sum . fmap nodestatLat . catMaybes) infosD
      avgLatD  = (\a b -> if b == 0 then NPSNoActiveNodes else NPSAvgLat $ a / fromIntegral b) <$> sumLatD <*> activeND
  valueOptionDyn $ NPSActiveNum <$> activeND
  descrOption $ NPSNodesNum $ length nodes
  descrOptionDyn avgLatD
  labelHorSep

ergNetworkWidget :: MonadFront t m => [NodeERG t] -> m ()
ergNetworkWidget nodes = do
  infosD <- fmap sequence $ traverse externalRefDynamic $ nodeconStatus <$> nodes
  let activeND = fmap (length . filter id) $ sequence $ nodeconIsUp <$> nodes
      sumLatD  = fmap (sum . fmap nodestatLat . catMaybes) infosD
      avgLatD  = (\a b -> if b == 0 then NPSNoActiveNodes else NPSAvgLat $ a / fromIntegral b) <$> sumLatD <*> activeND
  valueOptionDyn $ NPSActiveNum <$> activeND
  descrOption $ NPSNodesNum $ length nodes
  descrOptionDyn avgLatD
  labelHorSep

networkPageHeader :: MonadFront t m => Maybe Currency -> m (Dynamic t (Maybe (Currency, Event t ())))
networkPageHeader minitCur = do
  activeCursD <- getActiveCursD
  resD <- fmap join $ titleWrap $ widgetHoldDyn $ ffor activeCursD $ \curSet -> case S.toList curSet of
    [] -> do
      divClass "network-title-name" $ h3 $ localizedText NPSNoCurrencies
      pure $ pure $ Nothing
    cur:[] -> do
      divClass "network-title-name" $ h3 $ localizedText $ NPSTitleCur cur
      refrE <- divClass "network-title-cur" $ buttonClass "button button-outline net-refresh-btn" NPSRefresh
      pure $ pure $ Just (cur, refrE)
    curs -> do
      divClass "network-title-name" $ h3 $ localizedText $ NPSTitle
      curD <- divClass "network-title-cur" $ currenciesDropdown minitCur curs
      refrE <- divClass "network-title-cur" $ buttonClass "button button-outline net-refresh-btn" NPSRefresh
      pure $ (fmap . fmap) (, refrE) curD
  baseHorSep
  pure resD
  where
    titleWrap  = divClass "network-title-table" . divClass "network-title-row"
    baseHorSep = elAttr "hr" [("class","network-hr-sep"   )] blank
    currenciesDropdown :: MonadFrontBase t m => Maybe Currency -> [Currency] -> m (Dynamic t (Maybe Currency))
    currenciesDropdown minitKey currs = do
      langD <- getLanguage
      let listCursD = do
            l <- langD
            pure $ M.fromList $ fmap (\v -> (v, localizedShow l v)) currs
      let initKey = case minitKey of
            Nothing -> head currs
            Just k -> if k `elem` currs then k else head currs
      dp <- dropdown initKey listCursD $ def &
        dropdownConfig_attributes .~ constDyn ("class" =: "select-lang")
      (fmap . fmap) Just $ holdUniqDyn $ _dropdown_value dp

serversInfoPage :: MonadFront t m => Currency -> m ()
serversInfoPage initCur = do
  title <- localized NPSTitle
  wrapper False title (Just $ pure $ serversInfoPage initCur) $ mdo
    curD <- networkPageHeader $ Just initCur
    void $ widgetHoldDyn $ ffor curD $ maybe (pure ()) $ \(_, refrE) -> do
      connsD  <- externalRefDynamic =<< getActiveConnsRef
      setsD <- (fmap . fmap) settingsActiveAddrs getSettingsD
      let valD = (,) <$> connsD <*> setsD
      void $ widgetHoldDyn $ ffor valD $ \(conmap, urls) -> flip traverse urls $ \sa -> do
        let mconn = M.lookup sa conmap
        divClass "network-name" $ do
          let cls = if isJust mconn then "mt-a mb-a indexer-online" else "mt-a mb-a indexer-offline"
          elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
          text $ showt sa
        case mconn of
          Nothing -> pure ()
          Just conn -> do
            latD <- indexerConnPingerWidget conn refrE
            descrOptionDynNoBR $ NPSLatency <$> latD
      pure ()

lineOption :: MonadFront t m => m a -> m a
lineOption = divClass "network-wrapper"

nameOption, descrOption :: (MonadFront t m, LocalizedPrint a) => a -> m ()
nameOption = divClass "network-name"    . localizedText
descrOption = (>>) elBR . divClass "network-descr" . localizedText

valueOptionDyn, descrOptionDyn, descrOptionDynNoBR :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
descrOptionDyn v = getLanguage >>= \langD -> (>>) elBR (divClass "network-descr" $ dynText $ ffor2 langD v localizedShow)
descrOptionDynNoBR v = getLanguage >>= \langD -> divClass "network-descr" $ dynText $ ffor2 langD v localizedShow

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-lb")] blank
elBR = el "br" blank
