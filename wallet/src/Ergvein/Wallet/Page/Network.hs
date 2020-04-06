{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Network(
    networkPage
  ) where

import Control.Monad.IO.Class
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Servant.Client (BaseUrl, parseBaseUrl, showBaseUrl)

import Ergvein.Wallet.Client
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction (BlockHeight)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Currency
import Ergvein.Wallet.Localization.Network
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Wrapper

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

networkPage :: MonadFront t m => Maybe Currency -> m ()
networkPage curMb = wrapper NPSTitle (Just $ pure $ networkPage curMb) False $ do
  curD <- titleWrap $ do
    divClass "network-title-name" $ h3 $ localizedText $ NPSTitle
    divClass "network-title-cur" $ do
      let initCur = fromMaybe BTC curMb
      curE <- currenciesDropdown initCur allCurrencies
      holdDyn initCur curE
  baseHorSep
  void $ widgetHoldDyn $ ffor curD $ \cur -> optionsContent cur
  where
    titleWrap  = divClass "network-title" . divClass "network-title-table" . divClass "network-title-row"
    baseHorSep = elAttr "hr" [("class","network-hr-sep"   )] blank
    currenciesDropdown val allCurs = do
      langD <- getLanguage
      let curD = constDyn val
      initKey <- sample . current $ curD
      let listCursD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allCurs
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated curD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listCursD ddnCfg
      let selD = _dropdown_value dp
      fmap updated $ holdUniqDyn selD

optionsContent :: MonadFront t m => Currency -> m ()
optionsContent cur = do
  gpbE <- getPostBuild
  infomapD <- getIndexerInfoD
  let servNumD = (NPSServerVal . M.size) <$> infomapD
      actServNumD = (NPSServerVal . length . catMaybes . M.elems) <$> infomapD
      servCurInfoD = ffor infomapD $ \im -> let
        act = L.nub . catMaybes . fmap (M.lookup cur . indInfoHeights) . catMaybes . M.elems $ im
        in case act of
            [] ->  NPSNoServerAvail
            v:[] -> NPSSyncInfo v
            _ -> NPSDesync

  lineOption $ lineOptionNoEdit NPSServer servNumD NPSServerDescr
  lineOption $ lineOptionNoEdit NPSSyncStatus servCurInfoD NPSSyncDescr
  editE <- lineOption $ do
    refreshIndexerInfo =<< outlineButton NPSRefresh
    fmap (cur <$) $ outlineButton NPSServerList
  void $ nextWidget $ ffor editE $ \cur -> Retractable {
      retractableNext = pageSelectionOfServer cur
    , retractablePrev = Just (pure $ networkPage (Just cur))
    }

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

valueOptionDyn :: (MonadFront t m, LocalizedPrint a) => Dynamic t a -> m ()
valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow

labelHorSep, elBR :: MonadFront t m => m ()
labelHorSep = elAttr "hr" [("class","network-hr-sep-lb")] blank
elBR = el "br" blank

pageSelectionOfServer :: MonadFront t m => Currency -> m ()
pageSelectionOfServer cur = wrapper NPSTitle (Just $ pure $ pageSelectionOfServer cur) False $ mdo
  h3 $ localizedText NPSServerList
  infomapD <- fmap2 (M.mapKeys Just) getIndexerInfoD
  keyD <- holdDyn Nothing selE
  selE <- fmap2 snd $ selectViewListWithKey keyD infomapD renderItem
  void $ nextWidget $ ffor never $ \_ -> Retractable {
      retractableNext = networkPage $ Just cur
    , retractablePrev = Just $ pure $ networkPage $ Just cur
    }
  where
    renderItem :: MonadFront t m => Maybe BaseUrl -> Dynamic t (Maybe IndexerInfo) -> Dynamic t Bool -> m (Event t (Maybe BaseUrl))
    renderItem murl infoD selD = case murl of
      Nothing -> pure never
      Just url -> let selInfoD = (,) <$> selD <*> infoD in
        fmap (switch . current) $ widgetHoldDyn $ ffor selInfoD $ \(sel,minfo) -> do
          if sel then editWidget url minfo else infoWidget cur url minfo

editWidget :: MonadFront t m => BaseUrl -> Maybe IndexerInfo -> m (Event t (Maybe BaseUrl))
editWidget url minfo = lineOption $ do
  urlD <- fmap _textInput_value $ textInput def { _textInputConfig_initialValue = T.pack . showBaseUrl $ url }
  okE <- outlineButtonWithIconNoText "fas fa-check"
  cancelE <- outlineButtonWithIconNoText "fas fa-times"
  let murlE = attachWith (\t _ -> parseBaseUrl $ T.unpack t) (current urlD) okE
      dupE  = fforMaybe murlE $ \murl -> if Just url == murl then Just () else Nothing
      urlE  = fforMaybe murlE $ \murl -> if Just url == murl then Nothing else (url,) <$> murl
  updE <- updateIndexerURL urlE
  let urlErrE = fmapMaybe (maybe (Just NPSParseError) (const Nothing)) murlE
      updErrE = fmapMaybe (\b -> if b then Nothing else Just NPSDuplicateURL) updE
      errE    = leftmost [urlErrE, updErrE]
  noErrE <- delay 3 errE
  widgetHold (pure ()) $ ffor (leftmost [Nothing <$ noErrE, Just <$> errE]) $ \case
    Nothing -> pure ()
    Just err -> divClass "form-field-errors" $ localizedText err
  labelHorSep
  pure $ Nothing <$ (leftmost [cancelE, dupE])

infoWidget :: MonadFront t m => Currency -> BaseUrl -> Maybe IndexerInfo -> m (Event t (Maybe BaseUrl))
infoWidget cur url minfo = lineOption $ do
  (e,_) <- el "div" $ do
    divClass "network-name" $ do
      let cls = if isJust minfo then "indexer-online" else "indexer-offline"
      elClass "span" cls $ elClass "i" "fas fa-circle" $ pure ()
      text $ T.pack . showBaseUrl $ url
    elAttr' "button" [("class", "button button-outline network-name-edit")] $ localizedText NPSServerEdit
  maybe (pure ()) (descrOption . NPSLatency . indInfoLatency) minfo
  descrOption $ case minfo of
    Nothing -> NPSOffline
    Just info -> case M.lookup cur $ indInfoHeights info of
      Nothing -> NPSNoIndex cur
      Just v -> NPSHeightInfo v
  labelHorSep
  pure $ (Just url) <$ domEvent Click e

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
