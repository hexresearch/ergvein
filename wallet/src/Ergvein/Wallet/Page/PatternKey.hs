module Ergvein.Wallet.Page.PatternKey(
    patternKeyWidget
  ) where

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Wallet.Elements
import Ergvein.Text
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Storage.AuthInfo
import Reflex.Localize
import Data.Aeson.Types as A

import Reflex.Dom

import qualified Reflex                           as R
import qualified Reflex.Dom                       as RD
import qualified Reflex.Dom.Canvas.Context2D      as CanvasF
import qualified Reflex.Dom.CanvasBuilder.Types   as Canvas
import qualified Reflex.Dom.CanvasDyn             as CDyn


import qualified System.Random                    as Rnd
import           Control.Lens                     (to, (^.))
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Monoid
import           Data.Time                        (UTCTime, getCurrentTime)
import qualified Ergvein.Wallet.Page.PatternKeyUtils.Utils as UT


import Language.Javascript.JSaddle
import Data.Maybe

import qualified GHCJS.DOM.Blob as Blob
import qualified GHCJS.DOM.Types as JS


patternKeyWidget :: MonadFrontBase t m => m ()
patternKeyWidget = divClass "myTestDiv" $ do
  let
    canvasH = 320
    canvasW = 320
    dataN   = 20
    coords = zip [0..] $ reqList canvasW canvasW 3
    emptySq = zip (take 9 (repeat Nothing)) $ reqList canvasW canvasW 3

    canvasAttrs = Map.fromList
      [ ("height", T.pack . show $ canvasH)
      , ("width" , T.pack . show $ canvasW)
      , ("id"    , "fruity")
      ]

  aTime  <- liftIO $ getCurrentTime
  stdGen <- liftIO $ Rnd.getStdGen

  startE <- RD.button "Start"
  stopE  <- RD.button "Stop"

  (canvasEl, _) <- RD.elAttr' "canvas" canvasAttrs RD.blank

  let prepCoord (x,y) = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral x) - crLeft, (fromIntegral y) - crTop)) $ elementPosition (_element_raw canvasEl)
      prepTCoord TouchEventResult{..} = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral (_touchResult_screenX (head _touchEventResult_touches))) - crLeft, (fromIntegral (_touchResult_screenY (head _touchEventResult_touches)) - crTop - 35))) $ elementPosition (_element_raw canvasEl)
      tmoveE = domEvent Touchmove canvasEl
      tdownE  = domEvent Touchstart canvasEl
      tupE    = domEvent Touchend canvasEl
      moveE  = domEvent Mousemove canvasEl
      downE  = domEvent Mousedown canvasEl
      upE    = domEvent Mouseup canvasEl
    --  dGrid r  = constDyn $ drawGrid canvasW canvasH r
      dClear = constDyn $ clearCanvas canvasW canvasH
  tmovePrE <- performEvent $ ffor tmoveE prepTCoord
  tdownPrE <- performEvent $ ffor tdownE prepTCoord
  tupPrE   <- performEvent $ ffor tupE   prepTCoord
  movePrE  <- performEvent $ ffor moveE  prepCoord
  downPrE <- performEvent $ ffor downE prepCoord
  upPrE    <- performEvent $ ffor upE    prepCoord
  lastClickD <- holdDyn (0,0) downE
  --positionD <- holdDyn (0,0) $ fmap (lastClickD <- holdDyn (0,0) downE\(ClientRect{..}, _) -> (crLeft, crTop)) sizeE

  sqUpdE <- performEvent $ ffor tmovePrE $ \(x,y) -> pure ((x,y),hitOrMiss (x,y) coords)

  sqD <- holdDyn emptySq $ fmap (\(_,r) -> r) sqUpdE

--  dynText $ fmap showt sqD

  lD <- sample . current $ lastClickD

  eTick <- RD.tickLossy 0.016 aTime

  eTicken <- fmap R.switch . R.hold R.never $ R.leftmost
    [ ()      <$ eTick <$ startE
    , R.never <$ stopE
    ]

  --dLine <- holdDyn (drawLineZero) $ ffor movePrE $ \(x,y) -> do
  --  (fx, fy) <- liftM $ sample $ current $ lastClickDcrTop
  --    (a, b) <- liftM $ sample $ current $ positionD
  --  drawLineZero
  --  drawLine canvasW canvasH (fromIntegral x) (fromIntegral y) 0 0 -- (fromIntegral (fx - a)) (fromIntegral (fy - b))


  dLine <- holdDyn (drawLineZero) $ ffor sqUpdE $ \((x,y),r) -> do
    drawLine canvasW canvasH x y 0 0 r -- (fromIntegral (fx - a)) (fromIntegral (fy - b))
    --  (fx, fy) <- liftM $ sample $ current $ lastClickD
  --    (a, b) <- liftM $ sample $ current $ positionD


  d2D <- fmap (^. Canvas.canvasInfo_context) <$> CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )

--  eTick <- RD.tickLossy 0.016 aTime

--  eTicken <- fmap R.switch . R.hold R.never $ R.leftmost
--    [ ()      <$ eTick <$ eStart
--    , R.never <$ eStop
--    ]

--  dFloatFeed' <- UT.dFloatFeed ( 0.0, 450.0 ) stdGen eTicken
--  dDataLines <- UT.dDataz canvasH canvasW dataN
--    $ R.current dFloatFeed' <@ eTicken

--  _ <- CDyn.nextFrameWithCxFree dLine d2D $ () <$ tmovePrE
  _ <- CDyn.nextFrameWithCxFree dLine d2D $ () <$ eTicken

--  performEvent_ $ ffor startE $ \_ -> do
--    _ <- CDyn.nextFrameWithCxFree dGrid d2D startE

  _ <- CDyn.nextFrameWithCxFree dClear d2D stopE
  --divClass "myDebugLog" $ dynText $ fmap showt dDataLines

  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor downPrE $ \tR -> do
      text $ showt $ tR

  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor upPrE $ \tR -> do
      text $ showt $ tR

{-
  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor tdownE $ \tR -> do
      text $ showt $ tR

  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor tupE $ \tR -> do
      text $ showt $ tR

  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor tmovePrE $ \tR -> do
      text $ showt $ tR

  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor tdownPrE $ \tR -> do
      text $ showt $ tR

  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor tupE $ \tR -> do
      text $ showt $ tR
  --divClass "myDebugLog" $ dynText $ fmap showt dDataLines
  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor upE $ \tR -> do
      text $ showt $ tR
  --divClass "myDebugLog" $ dynText $ fmap showt dDataLines
  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor downE $ \tR -> dotR
      text $ showt $ tR
-}

  pure ()

hitOrMiss2 :: (Int, Int) -> [(Maybe Int, (Double, Double, Double, Double))] -> [(Maybe Int, (Double, Double, Double, Double))]
hitOrMiss2 (ix,iy) squares = fmap (\(num,(sqX, sqY, sqW, sqH)) -> if ((sqX < x) && (sqY < y) && ((sqX + sqW) > x) && ((sqY + sqH) > y))
  then (Just 1, (sqX, sqY, sqW, sqH))
  else (Nothing, (sqX, sqY, sqW, sqH))
  ) squares
  where
    x = fromIntegral ix
    y = fromIntegral iy

hitOrMiss :: (Double, Double) -> [(Int, (Double, Double, Double, Double))] -> [(Maybe Int, (Double, Double, Double, Double))]
hitOrMiss (x,y) squares = fmap (\(num,(sqX, sqY, sqW, sqH)) -> if ((sqX < x) && (sqY < y) && ((sqX + sqW) > x) && ((sqY + sqH) > y))
  then (Just num, (sqX, sqY, sqW, sqH))
  else (Nothing, (sqX, sqY, sqW, sqH))
  ) squares
--  where
--    x = fromIntegral ix
--    y = fromIntegral iy

clearCanvas :: Int -> Int -> CanvasF.CanvasM ()
clearCanvas canvasW canvasH = do
  CanvasF.clearRectF 0.0 0.0 (fromIntegral canvasW) (fromIntegral canvasH)
  CanvasF.beginPathF
  CanvasF.closePathF

drawGrid :: Int -> Int ->  [(Maybe Int, (Double, Double, Double, Double))] -> CanvasF.CanvasM ()
drawGrid canvasW canvasH r = do
  clearCanvas canvasW canvasH
  CanvasF.beginPathF
  CanvasF.rectF 0 0 (fromIntegral canvasW) (fromIntegral canvasH)
  traverse_ (\(mN, (a,b,c,d)) -> case mN of
    Just _ -> CanvasF.fillRectF (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)
    Nothing -> CanvasF.rectF (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)) r
  CanvasF.strokeStyleF "#000000"
  CanvasF.strokeF
  CanvasF.closePathF

drawLine :: Int -> Int -> Double -> Double -> Double -> Double -> [(Maybe Int, (Double, Double, Double, Double))] -> CanvasF.CanvasM ()
drawLine canvasW canvasH coordX coordY fromX fromY r = do
--  clearCanvas canvasW canvasH
--  CanvasF.beginPathF
  drawGrid canvasW canvasH r
  CanvasF.moveToF fromX fromY
  CanvasF.lineToF coordX coordY
  CanvasF.strokeStyleF "#000000"
  CanvasF.strokeF

drawLineZero :: CanvasF.CanvasM ()
drawLineZero = do
  CanvasF.moveToF 0 0

reqList :: Int -> Int -> Int -> [(Double, Double, Double, Double)]
reqList width height count = mconcat $ fmap (\num -> rowList width height count num) rList
  where
    stepW = fromIntegral $ floor (rW / (3*rCount+1))
    stepH = fromIntegral $ floor (rH / (3*rCount+1))
    rList = fmap fromIntegral $ [0 .. (count-1)]
    rCount = fromIntegral (count - 1)
    rW = fromIntegral width
    rH = fromIntegral height

rowList :: Int -> Int -> Int -> Double -> [(Double, Double, Double, Double)]
rowList width height count globN = fmap (\num -> (stepH*2*num + stepH,stepW*2*globN + stepW, stepW,stepH)) rList
  where
    stepW = fromIntegral $ floor (rW / (3*rCount+1))
    stepH = fromIntegral $ floor (rH / (3*rCount+1))
    rList = fmap fromIntegral $ [0 .. (count-1)]
    rCount = fromIntegral (count - 1)
    rW = fromIntegral width
    rH = fromIntegral height

data ClientRect = ClientRect {
    crBottom :: !Double
  , crHeight :: !Double
  , crLeft   :: !Double
  , crRight  :: !Double
  , crTop    :: !Double
  , crWidth  :: !Double
  } deriving (Show)

instance FromJSON ClientRect where
  parseJSON = withObject "ClientRect" $ \o -> do
    crBottom <- o .: "bottom"
    crHeight <- o .: "height"
    crLeft   <- o .: "left"
    crRight  <- o .: "right"
    crTop    <- o .: "top"
    crWidth  <- o .: "width"
    pure ClientRect{..}

instance FromJSVal ClientRect where
  fromJSVal v = do
    av <- fromJSVal v
    case av of
      Nothing -> fail "Failed to convert ClientRect to aeson"
      Just a -> case fromJSON a of
        A.Error s -> fail $ "Failed to parse aeson ClientRect " ++ s
        A.Success b -> pure $ Just b

elementPosition :: MonadJSM m => RawElement GhcjsDomSpace -> m ClientRect
elementPosition el = liftJSM $ do
  eval ("ergvein_elementPosition = function(a) { return a.getBoundingClientRect(); }" :: Text)
  jsv <- liftJSM $ jsg1 ("ergvein_elementPosition" :: Text) (toJSVal el)
  fromJSValUnchecked jsv
