{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.Settings(
    settingsPage
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Time
import Data.Function.Flip (flip3)
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
    pinCodeE <- graphPinCode
    pinCodeD <- holdDyn (PinCode []) pinCodeE
    dynText $ fmap showt pinCodeD
    pure ()

data PinCode = PinCode { unPinCode :: [Int] } deriving (Eq, Show)

data PinProcess = PinProcess
  { pinProcess'flag :: Bool
  , pinProcess'pin  :: PinCode
  } deriving (Eq, Show)

data PinAct
  = PinStart
  | PinStop
  | PinAdd Int
  deriving Show

isEmptyPinCode :: PinCode -> Bool
isEmptyPinCode = null . unPinCode

initPinProcess :: PinProcess
initPinProcess = PinProcess False $ PinCode []

addPinProcess :: PinProcess -> Int -> PinProcess
addPinProcess pp@PinProcess{..} v =
  let upc = unPinCode pinProcess'pin in case canAdd upc v of
    True  -> pp {pinProcess'pin = PinCode (upc ++ [v]) }
    False -> pp
  where
    canAdd :: [Int] -> Int -> Bool
    canAdd [] v =
      pinProcess'flag
    canAdd xs v =
      if last xs == v
        then False
        else pinProcess'flag

graphPinCode :: MonadFrontBase t m => m (Event t PinCode)
graphPinCode = mdo
  (e, itemE) <- elAttr' "div" canvasAttrs $ do
    resE <- fmap leftmost $ mapM (\(n,x,y) -> elItem n x y) itemsGeom
    chResE <- fmap switchDyn $ divClass "graph-pin-code-glass" $
      widgetHold (pure never) $ ffor (updated pinProcessD) $ \pp -> do
        let pvs = unPinCode . pinProcess'pin $ pp
        chActE <- fmap leftmost $ mapM (\(n,x,y) -> elItemCheck pvs n x y) itemsGeom
        drawLines pvs
        pure chActE
    pure $ leftmost [resE, chResE]
  let pinActE = leftmost [ PinStart <$ domEvent Mousedown e
                         , PinStop  <$ domEvent Mouseup   e
                         , itemE
                         ]
  pinProcessD' <- flip3 foldDyn pinActE initPinProcess $
                    \act pp -> case act of
                      PinStart -> initPinProcess {pinProcess'flag = True}
                      PinStop  -> pp {pinProcess'flag = False}
                      PinAdd v -> addPinProcess pp v
  pinProcessD <- holdUniqDyn pinProcessD'
  pure $ fforMaybe (updated pinProcessD) $ \PinProcess{..} ->
    if pinProcess'flag == False && isEmptyPinCode pinProcess'pin == False
      then Just pinProcess'pin
      else Nothing
  where
    itemsGeom :: [(Int,Int,Int)]
    itemsGeom = [ (1, sizeGrid  , sizeGrid  )
                , (2, sizeGrid*3, sizeGrid  )
                , (3, sizeGrid*5, sizeGrid  )
                , (4, sizeGrid  , sizeGrid*3)
                , (5, sizeGrid*3, sizeGrid*3)
                , (6, sizeGrid*5, sizeGrid*3)
                , (7, sizeGrid  , sizeGrid*5)
                , (8, sizeGrid*3, sizeGrid*5)
                , (9, sizeGrid*5, sizeGrid*5)
                ]
    canvasWidth, canvasHeight, sizeGrid :: Int
    canvasWidth = 420
    canvasHeight = 420
    sizeGrid = 420 `div` 6

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

    itemAttrs :: Int -> Int -> Int -> Int -> Text -> M.Map Text Text
    itemAttrs nmb posX posY whVal pref =
      [ ("id"   , "id_graph_pin_item_" <> pref <> showt nmb)
      , ("class", "graph-pin-code-" <> pref                )
      , ("style", itemStyle posX posY whVal                )
      ]

    itemStyle :: Int -> Int -> Int -> Text
    itemStyle posX posY whVal = "left:"   <> showt (posX - whVal `div` 2) <> "px" <> ";"
                             <> "top:"    <> showt (posY - whVal `div` 2) <> "px" <> ";"
                             <> "width:"  <> showt whVal <> "px" <> ";"
                             <> "height:" <> showt whVal <> "px" <> ";"

    elItem :: MonadFrontBase t m => Int -> Int -> Int -> m (Event t PinAct)
    elItem nmb posX posY = do
      (e,_) <- elAttr' "div" (itemAttrs nmb posX posY (2*itemR) "point") blank
      let downE  = domEvent Mousedown  e
          enterE = domEvent Mouseenter e
          startE = PinStart   <$ downE
          addE   = PinAdd nmb <$ enterE
      addAtStartE <- delay 0.01 $ PinAdd nmb <$ downE
      pure $ leftmost [startE, addAtStartE, addE]

    elItemCheck :: MonadFrontBase t m => [Int] -> Int -> Int -> Int -> m (Event t PinAct)
    elItemCheck pvs nmb posX posY =
      if elem nmb pvs == True
        then do
          (e,_) <- elAttr' "div" (itemAttrs nmb posX posY (3*itemR + itemR `div` 2) "point-check") blank
          let enterE = domEvent Mouseenter e
              addE   = PinAdd nmb <$ enterE
          pure addE
        else pure never

    drawLines :: MonadFrontBase t m => [Int] -> m ()
    drawLines = \case
      []         ->
        pure ()
      (x1:x2:xs) -> do
        let posX = fst $ getPosXY (x1 - 1)
            posY = snd $ getPosXY (x1 - 1)
        case calcAngle x1 x2 of
          Just (va,dx,dy) -> elAttr "div" [ ("class","graph-pin-code-line-check")
                                          , ("style","width:"     <> showt (sizeGrid*2) <> "px"       <> ";"
                                                  <> "left:"      <> showt (posX + dx)  <> "px"       <> ";"
                                                  <> "top:"       <> showt (posY + dy)  <> "px"       <> ";"
                                                  <> "transform:" <> " rotate(" <> showt va <> "deg)" <> ";"
                                            )
                                          ] blank
          Nothing -> pure ()
        drawLines (x2:xs)
      _         ->
        pure ()
      where
        calcAngle :: Int -> Int -> Maybe (Int, Int, Int)
        calcAngle v1 v2 = case v2 - v1 of
          (1 )  -> Just ( 0 ,         0  ,         0)
          (3 )  -> Just ( 90, -sizeGrid  ,  sizeGrid)
          (-1)  -> Just ( 0 , -sizeGrid*2,         0)
          (-3)  -> Just ( 90, -sizeGrid  , -sizeGrid)
          (4 )  -> Just ( 45,         0  ,  sizeGrid)
          (-4)  -> Just ( 45, -sizeGrid*2, -sizeGrid)
          (-2)  -> Just (-45,         0  , -sizeGrid)
          (2 )  -> Just (-45, -sizeGrid*2,  sizeGrid)
          _     -> Nothing

        getPosXY :: Int -> (Int,Int)
        getPosXY nmb = let (_,x,y) = itemsGeom !! nmb in (x,y)

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
