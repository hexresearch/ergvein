{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import qualified Data.Map.Strict as M
import Reflex.Dom
import qualified Reflex.Dom.Canvas.Context2D    as CanvasF
import qualified Reflex.Dom.CanvasBuilder.Types as Canvas
import qualified Reflex.Dom.CanvasDyn           as CDyn

import Ergvein.Text
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Wrapper

data SubPageSettings
  = GoLanguage
  | GoPinCode

settingsPage :: MonadFront t m => m ()
settingsPage = do
  let thisWidget = Just $ pure $ settingsPage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    divClass "initial-options grid1" $ do
      goLangE <- fmap (GoLanguage <$) $ outlineButton STPSButLanguage
      goPinE  <- fmap (GoPinCode  <$) $ outlineButton STPSButPinCode
      let goE = leftmost [goLangE, goPinE]
      void $ nextWidget $ ffor goE $ \spg -> Retractable {
          retractableNext = case spg of
            GoLanguage  -> languagePage
            GoPinCode   -> pinCodePage
        , retractablePrev = thisWidget
        }

languagePage :: MonadFront t m => m ()
languagePage = do
  let thisWidget = Just $ pure $ languagePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSelectLanguage
    divClass "initial-options grid1" $ do
      langD <- getLanguage
      initKey <- sample . current $ langD
      let listLangsD = ffor langD $ \l -> M.fromList $ fmap (\v -> (v, localizedShow l v)) allLanguages
          ddnCfg = DropdownConfig {
                _dropdownConfig_setValue   = updated langD
              , _dropdownConfig_attributes = constDyn ("class" =: "select-lang")
              }
      dp <- dropdown initKey listLangsD ddnCfg
      let selD = _dropdown_value dp
      selE <- fmap updated $ holdUniqDyn selD
      widgetHold (pure ()) $ setLanguage <$> selE
      settings <- getSettings
      updateSettings $ ffor selE (\lng -> settings {settingsLang = lng})
      pure ()
    pure ()

pinCodePage :: MonadFront t m => m ()
pinCodePage = do
  let thisWidget = Just $ pure $ pinCodePage
  menuWidget STPSTitle thisWidget
  wrapper True $ do
    h3 $ localizedText $ STPSSetsPinCode
    --elAttr "div" [("style","width: 200px; height: 200px; background-color: #00ff00; margin-left: auto; margin-right: auto;")] blank
    --graphPinCode
    divClass "" $ text "MyTest"
    el "span" $ text "Span Text!"
    --graphPinCode
    pure ()

graphPinCode :: MonadFrontBase t m => m ()
graphPinCode = do
  elAttr "svg" [ ("style","display: block; width: 400px; height: 400px; margin-left: auto; margin-right: auto;")
               , ("width", "400"), ("height", "400"), ("viewBox","0 0 400 400")
               ] $ do
    elAttr "circle" [ ("cx","100")
                    , ("cy","100")
                    , ("r","25")
                    --, ("style","fill: #ff0000; stroke: #ff0000; stroke-width: 1;")
                    , ("stroke","green")
                    , ("fill","white")
                    , ("stroke-width","1")
                    ] blank
    elAttr "rect" [("x","50"), ("y","100"), ("width","200"), ("height","100"), ("style","fill: #ffc107; stroke: #e65100; stroke-width: 2;")] blank
    pure ()

--graphPinCodeiCs :: MonadFrontBase t m => m ()
--graphPinCodeCs = do
--  canvasEl <- elAttr "canvas" [ ("style","display: block; width: 400px; height: 400px; margin-left: auto; margin-right: auto;")
--                              , ("width", "400"), ("height", "400"), ("viewBox","0 0 400 400")
--                              ] blank

