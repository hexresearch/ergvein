{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import Reflex.Host.Class
import Reflex.Dom as RD
--import Reflex.Dom.Canvas.Context2D    as CanvasF
--import Reflex.Dom.CanvasBuilder.Types as Canvas
--import Reflex.Dom.CanvasDyn           as CDyn

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
    graphPinCode
    pure ()

graphPinCode :: MonadFrontBase t m => m ()
graphPinCode = do
  (e, _) <- elAttr' "div" canvasAttrs $ do
    elItem 1 sizeGrid     sizeGrid
    elItem 2 (sizeGrid*3) sizeGrid
    elItem 3 (sizeGrid*5) sizeGrid
    elItem 4 sizeGrid     (sizeGrid*3)
    elItem 5 (sizeGrid*3) (sizeGrid*3)
    elItem 6 (sizeGrid*5) (sizeGrid*3)
    elItem 7 sizeGrid     (sizeGrid*5)
    elItem 8 (sizeGrid*3) (sizeGrid*5)
    elItem 9 (sizeGrid*5) (sizeGrid*5)
    pure ()
  let xyE = domEvent Mousemove e
  xyD <- holdDyn (0,0) xyE
  dynText $ fmap showt xyD
  pure ()
  where
    canvasWidth, canvasHeight, sizeGrid :: Int
    canvasWidth = 420
    canvasHeight = 420
    sizeGrid = 400 `div` 6

    itemR :: Int
    itemR = sizeGrid `div` 4

    canvasAttrs :: M.Map Text Text
    canvasAttrs = [ ("id"   , "id_graph_pin_code_canvas")
                  , ("class", "graph-pin-code-canvas"   )
                  , ("style", canvasStyle               )
                  ]

    canvasStyle :: Text
    canvasStyle = "width:"  <> showt canvasWidth  <> "px" <> ";"
               <> "height:" <> showt canvasHeight <> "px" <> ";"

    itemAttrs :: Int -> Int -> Int -> M.Map Text Text
    itemAttrs nmb posX posY =
      [ ("id"   , "id_graph_pin_item_" <> showt nmb)
      , ("class", "graph-pin-code-point"           )
      , ("style", itemStyle posX posY              )
      ]

    itemStyle :: Int -> Int -> Text
    itemStyle posX posY = "left:"   <> showt posX      <> "px" <> ";"
                       <> "top:"    <> showt posY      <> "px" <> ";"
                       <> "width:"  <> showt (2*itemR) <> "px" <> ";"
                       <> "height:" <> showt (2*itemR) <> "px" <> ";"

    elItem :: MonadFrontBase t m => Int -> Int -> Int -> m ()
    elItem nmb posX posY =
      elAttr "div" (itemAttrs nmb posX posY) blank

{-
graphPinCode :: forall t m . (MonadFrontBase t m) => m ()
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

graphPinCodeCs :: MonadFront t m => m ()
graphPinCodeCs = do
  now <- liftIO getCurrentTime
  canvasEl <- fst <$> elAttr' "canvas"
                        [ ("style","display: block; width: 400px; height: 400px; margin-left: auto; margin-right: auto;")
                        ] blank
  d2D <- fmap Canvas._canvasInfo_context <$> CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )
  eTick <- fmap void $ RD.tickLossy 0.1 now
  _ <- CDyn.nextFrameWithCxFree (constDyn toDraw) d2D eTick
  pure ()
  where
    toDraw = do
      CanvasF.strokeStyleF "#FF0000"
      CanvasF.rectF 10 10 100 100
-}
