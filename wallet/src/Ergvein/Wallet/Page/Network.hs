{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Network(
    networkPage
  ) where

import qualified Data.Map.Strict as M

import Ergvein.Wallet.Client
import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
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
    nameOption NPSStatus
    valueOption $ NPSStatusVal 10
    descrOption NPSStatusDescr
    labelHorSep
  lineOption $ do
    nameOption NPSServer
    valueOption $ NPSServerVal "test.server.world.ru"
    descrOption NPSServerDescr
    labelHorSep
  lineOption $ do
    nameOption NPSHeight
    heightE' <- getHeight (HeightRequest cur <$ gpbE)
    let heightE = ffor heightE' $ \case
                    Left err    -> NPSError err
                    Right hrsp  -> NPSHeightVal $ heightRespHeight hrsp
    heightD <- holdDyn NPSWait heightE
    valueOptionDyn heightD
    descrOption NPSHeightDescr
    labelHorSep
  pure ()
  where
    lineOption       = divClass "network-wrapper" . divClass "network-line"
    nameOption       = divClass "network-name"    . localizedText
    valueOption      = divClass "network-value"   . localizedText
    valueOptionDyn v = getLanguage >>= \langD -> divClass "network-value" $ dynText $ ffor2 langD v localizedShow
    descrOption      = (>>) elBR . divClass "network-descr" . localizedText
    labelHorSep      = elAttr "hr" [("class","network-hr-sep-lb")] blank
    elBR             = el "br" blank
    --optionSection t v d = do
    --  nameOption t
    --  valueOption v
    --  descrOption d
    --  labelHorSep
