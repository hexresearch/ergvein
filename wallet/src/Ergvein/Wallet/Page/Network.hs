{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Network(
    networkPage
  ) where

import Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import Servant.Client (BaseUrl, parseBaseUrl)

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

networkPage :: MonadFront t m => m ()
networkPage = do
  let thisWidget = Just $ pure $ networkPage
  menuWidget NPSTitle thisWidget
  wrapper False $ do
    curD <- titleWrap $ do
      divClass "network-title-name" $ h3 $ localizedText $ NPSTitle
      divClass "network-title-cur" $ do
        curE <- currenciesDropdown BTC allCurrencies
        holdDyn BTC curE
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
  lineOption $ do
    statusD <- flip fmap (tempGetStatus cur) $ \sD -> ffor sD $ \case
                    Left err  -> NPSError err
                    Right v   -> NPSStatusVal v
    lineOptionNoEdit NPSStatus statusD NPSStatusDescr
  lineOption $ do
    nameOption NPSServer
    baseUrlD <- fmap (NPSServerVal <$>) $ tempGetServer cur
    valueOptionDyn baseUrlD
    descrOption NPSServerDescr
    labelHorSep
  lineOption $ do
    heightD <- flip fmap (tempGetHeight cur) $ \hD -> ffor hD $ \case
                 Left err   -> NPSError err
                 Right hrsp -> NPSHeightVal hrsp
    lineOptionNoEdit NPSHeight heightD NPSHeightDescr
  pure ()
  where
    lineOption       = divClass "network-wrapper" . divClass "network-line"
    nameOption       = divClass "network-name"    . localizedText
    --valueOption      = divClass "network-value"   . localizedText
    valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
    descrOption      = (>>) elBR . divClass "network-descr" . localizedText
    labelHorSep      = elAttr "hr" [("class","network-hr-sep-lb")] blank
    elBR             = el "br" blank

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
  where
    nameOption       = divClass "network-name"    . localizedText
    valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
    descrOption      = (>>) elBR . divClass "network-descr" . localizedText
    labelHorSep      = elAttr "hr" [("class","network-hr-sep-lb")] blank
    elBR             = el "br" blank


-- | Temporary stubs for data
data TempErr =
    TempErr
  | TempErrNoData

instance LocalizedPrint TempErr where
  localizedShow l v = case l of
    English -> case v of
      TempErr       -> "Error"
      TempErrNoData -> "It is not possibleto receive data"
    Russian -> case v of
      TempErr       -> "Ошибка"
      TempErrNoData -> "Невозможно получить данные"

tempGetStatus:: MonadFront t m => Currency -> m (Dynamic t (Either TempErr Int))
tempGetStatus = \case
  BTC   -> pure $ pure $ Right 1
  ERGO  -> pure $ pure $ Left TempErr

tempGetHeight :: MonadFront t m => Currency -> m (Dynamic t (Either TempErr BlockHeight))
tempGetHeight = \case
  BTC   -> pure $ pure $ Right 5
  ERGO  -> pure $ pure $ Right 3

tempGetServer :: MonadFront t m => Currency -> m (Dynamic t BaseUrl)
tempGetServer = \case
  BTC   -> do baseUrl <- liftIO $ parseBaseUrl "http://test.serverbtc.ru"
              pure $ pure baseUrl
  ERGO  -> do baseUrl <- liftIO $ parseBaseUrl "http://test.serverergo.ru"
              pure $ pure baseUrl
