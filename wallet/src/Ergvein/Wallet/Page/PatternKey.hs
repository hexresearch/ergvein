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


import           Control.Lens                     (to, (^.))
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.List  (find)
import           Data.List.Split
import           Data.Monoid
import qualified Data.Text as T
import           Data.Time                        (UTCTime, getCurrentTime)
import qualified Ergvein.Wallet.Page.PatternKeyUtils.Utils as UT


import Language.Javascript.JSaddle hiding ((!!))
import Data.Maybe

import qualified GHCJS.DOM.Blob as Blob
import qualified GHCJS.DOM.Types as JS

type Square  = (Double, Double, Double, Double)
type Coursor = (Double, Double)

data DrawCommand = AddSquare | Clear deriving (Show)

data TouchState = Pressed | Unpressed deriving (Show)

patternKeyWidget :: MonadFrontBase t m => m ()
patternKeyWidget = divClass "myTestDiv" $ mdo
  let
    canvasAttrs = Map.fromList
      [ ("height", T.pack . show $ canvasH)
      , ("width" , T.pack . show $ canvasW)
      , ("id"    , "fruity")
      ]

  aTime  <- liftIO $ getCurrentTime

  buildE <- delay 0.1 =<< getPostBuild

  (canvasEl, _) <- RD.elAttr' "canvas" canvasAttrs RD.blank

  let elP = elementPosition $ _element_raw canvasEl

  let prepCoord (x,y) = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral x) - crLeft, (fromIntegral y) - crTop)) elP
      prepTCoord TouchEventResult{..} = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral (_touchResult_screenX (head _touchEventResult_touches))) - crLeft, (fromIntegral (_touchResult_screenY (head _touchEventResult_touches)) - crTop - 35))) elP
      tmoveE = domEvent Touchmove canvasEl
      tdownE  = domEvent Touchstart canvasEl
      tupE    = domEvent Touchend canvasEl
      moveE  = domEvent Mousemove canvasEl
      downE  = domEvent Mousedown canvasEl
      upE    = domEvent Mouseup canvasEl
      --dGrid  = constDyn $ drawGrid canvasW canvasH emptySq
--      dClear = constDyn $ clearCanvas canvasW canvasH
      pressedE = leftmost [Pressed <$ tdownE, Unpressed <$ tupE]
      predrawE = leftmost [sqUpdE, (Clear,(0,0),emptySq) <$ tupPrE]
      selE = fmap (\(dc, sqs) -> (dc, fmap fst sqs)) $ fmap (\(dc,sqs) -> (dc, filter jstFilter sqs)) $ fmap (\(dc,_,sqs) -> (dc,sqs)) predrawE
  tmovePrE <- performEvent $ ffor tmoveE prepTCoord
  tdownPrE <- performEvent $ ffor tdownE prepTCoord
  tupPrE   <- performEvent $ ffor tupE   prepTCoord
  movePrE  <- performEvent $ ffor moveE  prepCoord
  downPrE  <- performEvent $ ffor downE  prepCoord
  upPrE    <- performEvent $ ffor upE    prepCoord
  touchD <- holdDyn Unpressed pressedE
  sqUpdE <- performEvent $ ffor (leftmost [tmovePrE, tdownPrE]) $ \(x,y) -> pure (AddSquare,(x,y),hitOrMiss (x,y) coords)

  dGridT  <- holdDyn (drawGridT canvasW canvasH emptySq) $ never

  sqD <- holdDyn (Clear,(0,0),emptySq) $ flip pushAlways predrawE $ \(dc,cur,sqs) -> do
    touchS <- sample . current $ touchD
    case touchS of
      Pressed -> case dc of
        AddSquare -> do
          (_,_,sqsv) <- sample . current $ sqD
          pure (AddSquare,cur,concatMyLists sqs sqsv)
        Clear -> pure (Clear,(0,0),emptySq)
      Unpressed -> pure (Clear,(0,0),emptySq)

  selectedD <- holdDyn (Clear,[]) $ flip pushAlways selE $ \(dc, sqs) -> do
    touchS <- sample . current $ touchD
    case touchS of
      Pressed -> case dc of
        AddSquare -> if (null sqs)
          then do
            v <- sample . current $ selectedD
            pure v
          else do
            (dc,v) <- sample . current $ selectedD
            case (find (==(head sqs)) v) of
              Just _ -> pure (AddSquare, v)
              Nothing -> pure $ (AddSquare, v <> [head sqs])
        Clear -> pure (Clear, [])
      Unpressed -> pure (Clear, [])


  let draw2E = updated selectedD
  let draw2E' = fmap (\(a,lst) -> case a of
        Clear -> (Clear,(0,0))
        AddSquare -> if null lst
          then (AddSquare,(0,0))
          else (AddSquare,(sqr2tuple (lst :: [Maybe Int])))
            ) draw2E

  moveD <- holdDyn (Clear,(0,0)) draw2E'

  draw1E <- performEvent $ ffor (updated sqD) $ \a -> do
    sel <- sample . current $ selectedD
    ln <- sample . current $ moveD
    pure (a,sel,ln)

  dLineT <- holdDyn (drawLineZeroT) $ ffor draw1E $ \((_,(x,y),r),sel,ln) -> (drawLineT canvasW canvasH x y 0 0 ln r)
                                                                          <> (drawLinesT sel coords)
{-
  dLine <- holdDyn (drawLineZero) $ ffor draw1E $ \((_,(x,y),r),sel,ln) -> do
    drawLine canvasW canvasH x y 0 0 ln r
    drawLines sel coords

  d2D <- fmap (^. Canvas.canvasInfo_context) <$> CDyn.dContext2d (Canvas.CanvasConfig canvasEl [])
  _ <- CDyn.nextFrameWithCxFree dGrid d2D $ leftmost [() <$ tdownE, () <$ tupE, buildE]
  _ <- CDyn.nextFrameWithCxFree dLine d2D $ () <$ tmoveE

-}
  performEvent_ $ ffor (leftmost [() <$ tdownE, () <$ tupE, buildE]) $ \_ -> do
    dGridS <- sample . current $ dGridT
    rawJSCall (_element_raw canvasEl) dGridS

  performEvent_ $ ffor (leftmost [() <$ tmoveE, () <$ tdownE]) $ \_ -> do
    dLineS <- sample . current $ dLineT
    rawJSCall (_element_raw canvasEl) dLineS

  pure ()
    where
      sqr2tuple :: [Maybe Int] -> (Double, Double)
      sqr2tuple lst = case (last lst) of
        Just num -> (\(a,b,c,d) -> (a+c/2,b+d/2)) $ snd $ emptySq !! num
        Nothing -> (0,0)

      canvasH = 320
      canvasW = 320
      coords = zip [0..] $ reqList canvasW canvasW 3
      emptySq = zip (take 9 (repeat Nothing)) $ reqList canvasW canvasW 3

jstFilter :: (Maybe Int, a) -> Bool
jstFilter a = case (fst a) of
  Just n -> True
  Nothing -> False

concatMyLists :: [(Maybe Int, Square)] -> [(Maybe Int, Square)] -> [(Maybe Int, Square)]
concatMyLists a b = (\((mi1,f1),(mi2,f2)) -> case mi1 of
  Just n1 -> (Just n1, f1)
  Nothing -> case mi2 of
    Just n2 -> (Just n2, f2)
    Nothing -> (Nothing,f2)
  ) <$> (zip a b)

hitOrMiss :: Coursor -> [(Int, Square)] -> [(Maybe Int, Square)]
hitOrMiss (x,y) squares = fmap (\(num,(sqX, sqY, sqW, sqH)) -> if (((sqX-5) < x) && ((sqY-5) < y) && ((sqX + sqW + 5) > x) && ((sqY + sqH + 5) > y))
  then (Just num, (sqX, sqY, sqW, sqH))
  else (Nothing, (sqX, sqY, sqW, sqH))
  ) squares

clearCanvas :: Int -> Int -> CanvasF.CanvasM ()
clearCanvas canvasW canvasH = do
  CanvasF.clearRectF 0.0 0.0 (fromIntegral canvasW) (fromIntegral canvasH)

drawGrid :: Int -> Int ->  [(Maybe Int, Square)] -> CanvasF.CanvasM ()
drawGrid canvasW canvasH r = do
  clearCanvas canvasW canvasH
  CanvasF.beginPathF
  CanvasF.rectF 0 0 (fromIntegral canvasW) (fromIntegral canvasH)
  traverse_ (\(mN, (a,b,c,d)) -> case mN of
    Just _ -> CanvasF.fillRectF (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)
    Nothing -> CanvasF.rectF (realToFrac a) (realToFrac b) (realToFrac c) (realToFrac d)) r
  CanvasF.strokeStyleF "#000000"
  CanvasF.strokeF

drawGridT :: Int -> Int -> [(Maybe Int, Square)] -> Text
drawGridT cW cH r = (clearCanvasT cW cH)
                    <> beginPathT
                    <> (rectZeroT cW cH)
                    <> (T.concat  (fmap fillRects r))
                    <> strokeStyleT
                    <> strokeT
  where
    fillRects (mN, (a,b,c,d)) = case mN of
      Just _ -> fillRectT a b c d
      Nothing -> rectT a b c d

clearCanvasT :: Int -> Int -> Text
clearCanvasT cW cH = " ctx.clearRect(0,0," <> (showt cW) <> "," <> (showt cH) <> "); "

beginPathT :: Text
beginPathT = " ctx.beginPath(); "

strokeStyleT :: Text
strokeStyleT = " ctx.strokeStyle = \"#000000\"; "

strokeT :: Text
strokeT = " ctx.stroke(); "

rectZeroT :: Int -> Int -> Text
rectZeroT cW cH = " ctx.rect(0,0," <> (showt cW) <> "," <> (showt cH) <> "); "

fillRectT :: Double -> Double -> Double -> Double -> Text
fillRectT a b c d = " ctx.fillRect(" <> (showt a) <> "," <> (showt b) <> "," <> (showt c) <> "," <> (showt d) <> "); "

rectT :: Double -> Double -> Double -> Double -> Text
rectT a b c d = " ctx.rect(" <> (showt a) <> "," <> (showt b) <> "," <> (showt c) <> "," <> (showt d) <> "); "

lineWidthT :: Int -> Text
lineWidthT lw = " ctx.lineWidth = " <> (showt lw) <> "; "

moveToT :: Double -> Double -> Text
moveToT mX mY = " ctx.moveTo(" <> (showt mX) <> "," <> (showt mY) <> "); "

lineToT :: Double -> Double -> Text
lineToT mX mY = " ctx.lineTo(" <> (showt mX) <> "," <> (showt mY) <> "); "

drawLineT :: Int -> Int -> Double -> Double -> Double -> Double -> (DrawCommand,(Double,Double)) -> [(Maybe Int, Square)] -> Text
drawLineT canvasW canvasH coordX coordY fromX fromY (a,(cntX,cntY)) r = case a of
  Clear ->  drawGridT canvasW canvasH r
  AddSquare -> (drawGridT canvasW canvasH r)
            <> (moveToT cntX cntY)
            <> (lineToT coordX coordY)
            <> strokeT

drawLine :: Int -> Int -> Double -> Double -> Double -> Double -> (DrawCommand,(Double,Double)) -> [(Maybe Int, Square)] -> CanvasF.CanvasM ()
drawLine canvasW canvasH coordX coordY fromX fromY (a,(cntX,cntY)) r = do
  drawGrid canvasW canvasH r
  CanvasF.lineWidthF 2
  case a of
    Clear -> do
      CanvasF.strokeStyleF "#000000"
      CanvasF.strokeF
    AddSquare -> do
      CanvasF.moveToF cntX cntY
      CanvasF.lineToF coordX coordY
      CanvasF.strokeStyleF "#000000"
      CanvasF.strokeF

drawLines :: (DrawCommand, [Maybe Int]) -> [(Int, Square)] -> CanvasF.CanvasM ()
drawLines (dc, mi) z = case dc of
  AddSquare -> if ((length mi) < 2)
    then pure ()
    else do
      let (fjMi :: [Int]) = fmap fromJust mi
      let (prepList :: [Int]) = ([head fjMi]) <> (concat (fmap (\a -> [a,a]) fjMi)) <> ([last fjMi])
      let pointsList  = chunksOf 2 $ fmap (\a -> case (find (\(num,_) -> num == a ) z) of
            Just (num, (a,b,c,d)) -> (a+c/2,b+d/2)
            Nothing -> (0,0) ) prepList
      traverse_ (\[(ax,ay),(bx,by)] -> do
        CanvasF.beginPathF
        CanvasF.lineWidthF 2
        CanvasF.moveToF ax ay
        CanvasF.lineToF bx by
        CanvasF.strokeStyleF "#000000"
        CanvasF.strokeF

         ) (pointsList :: [[(Double,Double)]])
      pure ()
  Clear -> pure ()

drawLinesT :: (DrawCommand, [Maybe Int]) -> [(Int, Square)] -> Text
drawLinesT (dc, mi) z = case dc of
  AddSquare -> if ((length mi) < 2)
    then ""
    else T.concat $ fmap drawLs pointsList
      where
        (fjMi :: [Int]) = fmap fromJust mi
        (prepList :: [Int]) = ([head fjMi]) <> (concat (fmap (\a -> [a,a]) fjMi)) <> ([last fjMi])
        pointsList  = chunksOf 2 $ fmap (\a -> case (find (\(num,_) -> num == a ) z) of
            Just (num, (a,b,c,d)) -> (a+c/2,b+d/2)
            Nothing -> (0,0) ) prepList
        drawLs :: [(Double, Double)] -> Text
        drawLs [(ax,ay),(bx,by)] = beginPathT
                                <> (lineWidthT 2)
                                <> (moveToT ax ay)
                                <> (lineToT bx by)
                                <> strokeT
  Clear -> ""

drawLineZeroT :: Text
drawLineZeroT = " ctx.moveTo(0,0); "

drawLineZero :: CanvasF.CanvasM ()
drawLineZero = do
  CanvasF.moveToF 0 0

reqList :: Int -> Int -> Int -> [Square]
reqList width height count = mconcat $ fmap (\num -> rowList width height count num) rList
  where
    stepW = fromIntegral $ floor (rW / (3*rCount+1))
    stepH = fromIntegral $ floor (rH / (3*rCount+1))
    rList = fmap fromIntegral $ [0 .. (count-1)]
    rCount = fromIntegral (count - 1)
    rW = fromIntegral width
    rH = fromIntegral height

rowList :: Int -> Int -> Int -> Double -> [Square]
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
{-
  CanvasF.beginPathF
  CanvasF.lineWidthF 2
  CanvasF.moveToF ax ay
  CanvasF.lineToF bx by
  CanvasF.strokeStyleF "#000000"
  CanvasF.strokeF
-}

rawJSBeginPath :: MonadJSM m => RawElement GhcjsDomSpace -> m ()
rawJSBeginPath el = liftJSM $ do
  eval func1
  _ <- liftJSM $ jsg1 func2 (toJSVal el)
  pure ()
  where
    (func2 :: Text) = "ergvein_drawtouchline"
    (func1 :: Text) = " ergvein_drawtouchline = function(cnv) { "
                   <> " var ctx = cnv.getContext(\"2d\");"
                   <> " ctx.beginPath(); "
                   <> " ctx.lineWidth = 2; "
                   <> " ctx.moveTo(0,0); "
                   <> " ctx.lineTo(100,100); "
                   <> " ctx.strokeStyle = \"#000000\"; "
                   <> " ctx.stroke(); "
                   <> " }"

rawJSCall :: MonadJSM m => RawElement GhcjsDomSpace -> Text -> m ()
rawJSCall el t = liftJSM $ do
  eval ("console.log('called!')"::Text)
  eval func1
  _ <- liftJSM $ jsg1 func2 (toJSVal el)
  pure ()
  where
    (func2 :: Text) = "ergvein_drawgrid"
    (func1 :: Text) = " ergvein_drawgrid = function(cnv) { " <> " var ctx = cnv.getContext(\"2d\");" <> t <> " }"
