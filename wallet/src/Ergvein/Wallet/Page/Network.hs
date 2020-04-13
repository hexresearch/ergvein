{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Network(
    networkPage
  ) where

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Servant.Client (BaseUrl, parseBaseUrl, showBaseUrl)

import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction (BlockHeight)
import Ergvein.Wallet.Client
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Currency
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Node
import Ergvein.Wallet.Wrapper

import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

import Ergvein.Wallet.Native

networkPage :: MonadFront t m => Maybe Currency -> m ()
networkPage curMb = wrapper NPSTitle (Just $ pure $ networkPage curMb) False $ do
  curD <- networkPageHeader curMb
  void $ widgetHoldDyn $ ffor curD $ \case
    Nothing -> pure ()
    Just cur -> networkPageWidget cur


networkPageWidget :: MonadFront t m => Currency -> m ()
networkPageWidget cur = do
  allIndsD <- getIndexerInfoD
  conmapD <- getNodeConnectionsD
  let currInfoD = ffor allIndsD $ \im -> let
        act = catMaybes $ M.elems im
        in catMaybes $ ffor act $ \IndexerInfo{..} -> (indInfoLatency,) <$> M.lookup cur indInfoHeights
      servCurInfoD = ffor currInfoD $ \xys -> let
        act = (\(_,ys) -> L.nub ys) $ unzip xys
        in case act of
            [] ->  NPSNoServerAvail
            v:[] -> NPSSyncInfo v
            _ -> NPSDesync
      (servNumD, avgLatD) = splitDynPure $ ffor currInfoD $ \xys -> let
        l = length xys
        lats = fst $ unzip xys
        avgL = if l == 0 then NPSNoServerAvail else NPSAvgLat $ sum lats / fromIntegral l
        in (NPSServerVal l, avgL)

  listE <- lineOption $ do
    nameOption NPSServer
    listE <- el "div" $ do
      valueOptionDyn servNumD
      divClass "network-name-edit" $ fmap (cur <$) $ outlineButton NPSServerListView
    descrOption NPSServerDescr
    descrOptionDyn avgLatD
    labelHorSep
    pure listE

  lineOption $ lineOptionNoEdit NPSSyncStatus servCurInfoD NPSSyncDescr
  lineOption $ do
    let nnnD = ffor conmapD $ \cm -> case cur of
          BTC  -> let
            btcm    = fromMaybe M.empty $ DM.lookup BTCTag cm
            actives = catMaybes $ fmap nodeconStatus $ M.elems btcm
            actN    = length actives
            avgLat  = if actN == 0 then NPSNoActiveNodes else NPSAvgLat $ (sum $ fmap nodestatLat actives) / fromIntegral actN
            in (NPSNodesNum $ M.size btcm, NPSActiveNum actN, avgLat)
          ERGO  -> let
            ergom   = fromMaybe M.empty $ DM.lookup ERGOTag cm
            actives = catMaybes $ fmap nodeconStatus $ M.elems ergom
            actN    = length actives
            avgLat  = if actN == 0 then NPSNoActiveNodes else NPSAvgLat $ (sum $ fmap nodestatLat actives) / fromIntegral actN
            in (NPSNodesNum $ M.size ergom, NPSActiveNum actN, avgLat)
        totalND  = (\(n,_,_) -> n) <$> nnnD
        activeND = (\(_,n,_) -> n) <$> nnnD
        avgLatD  = (\(_,_,l) -> l) <$> nnnD
    nameOption NPSNodes
    valueOptionDyn activeND
    descrOptionDyn totalND
    descrOptionDyn avgLatD
    labelHorSep
  void $ nextWidget $ ffor listE $ \cur -> Retractable {
      retractableNext = serversInfoPage cur
    , retractablePrev = Just (pure $ networkPage (Just cur))
    }

networkPageHeader :: MonadFront t m => Maybe Currency -> m (Dynamic t (Maybe Currency))
networkPageHeader minitCur = do
  activeCursD <- getActiveCursD
  langD <- getLanguage
  curD <- fmap join $ titleWrap $ widgetHoldDyn $ ffor activeCursD $ \curSet -> case S.toList curSet of
    [] -> do
      divClass "network-title-name" $ h3 $ localizedText NPSNoCurrencies
      pure $ pure Nothing
    cur:[] -> do
      divClass "network-title-name" $ h3 $ localizedText $ NPSTitleCur cur
      divClass "network-title-cur" $ refreshIndexerInfo =<< buttonClass "button button-outline net-refresh-btn" NPSRefresh
      pure $ pure (Just cur)
    curs -> do
      divClass "network-title-name" $ h3 $ localizedText $ NPSTitle
      curD <- divClass "network-title-cur" $ currenciesDropdown minitCur curs
      divClass "network-title-cur" $ refreshIndexerInfo =<< buttonClass "button button-outline net-refresh-btn" NPSRefresh
      pure curD
  baseHorSep
  pure curD
  where
    titleWrap  = divClass "network-title" . divClass "network-title-table" . divClass "network-title-row"
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
serversInfoPage initCur = wrapper NPSTitle (Just $ pure $ serversInfoPage initCur) False $ mdo
  curD <- networkPageHeader $ Just initCur
  void $ widgetHoldDyn $ ffor curD $ \case
    Nothing -> pure ()
    Just cur -> do
      allIndsD <- getIndexerInfoD
      let indMapD = fmap (M.filter (isJust . join . fmap (M.lookup cur . indInfoHeights))) allIndsD
      void $ listWithKey indMapD $ \url minfoD -> lineOption $ widgetHoldDyn $ ffor minfoD $ \minfo -> do
        divClass "network-name" $ do
          let cls = if isJust minfo then "indexer-online" else "indexer-offline"
          elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
          text $ T.pack . showBaseUrl $ url
        maybe (pure ()) (descrOption . NPSLatency . indInfoLatency) minfo
        descrOption $ maybe NPSOffline (maybe (NPSNoIndex cur) NPSHeightInfo . M.lookup cur . indInfoHeights) minfo
        labelHorSep

lineOptionNoEdit :: MonadFront t m
                 => NetworkPageStrings
                 -> Dynamic t NetworkPageStrings
                 -> NetworkPageStrings
                 -> m ()
lineOptionNoEdit name valD descr = do
  nameOption name
  valueOptionDyn valD
  descrOption descr
  labelHorSep

lineOption :: MonadFront t m => m a -> m a
lineOption = divClass "network-wrapper" . divClass "network-line"

nameOption, descrOption :: (MonadFront t m, LocalizedPrint a) => a -> m ()
nameOption = divClass "network-name"    . localizedText
descrOption = (>>) elBR . divClass "network-descr" . localizedText

valueOptionDyn, descrOptionDyn :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
descrOptionDyn v = getLanguage >>= \langD -> (>>) elBR (divClass "network-descr" $ dynText $ ffor2 langD v localizedShow)

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-lb")] blank
elBR = el "br" blank
