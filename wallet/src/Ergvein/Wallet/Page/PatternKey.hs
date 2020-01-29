module Ergvein.Wallet.Page.PatternKey(
    patternKeyWidget
  ) where

import Ergvein.Text
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import Data.Aeson.Types as A

import Reflex.Dom

import qualified Data.Map as Map
import           Data.Maybe (fromJust)
import           Data.List  (find)
import           Data.List.Split
import qualified Data.Text as T

import Language.Javascript.JSaddle hiding ((!!))

patternKeyWidget :: MonadFrontBase t m => m ()
patternKeyWidget = divClass "pattern-container" $ mdo
  buildE <- delay 0.1 =<< getPostBuild
  canvasEl <- createCanvas cOpts
  
  let elP = elementPosition $ _element_raw canvasEl
      prepCoord (x,y) = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral x) - crLeft, (fromIntegral y) - crTop)) elP
      prepTCoord TouchEventResult{..} = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral (_touchResult_screenX (head _touchEventResult_touches))) - crLeft, (fromIntegral (_touchResult_screenY (head _touchEventResult_touches)) - crTop - 35))) elP
      tmoveE = domEvent Touchmove canvasEl
      tdownE  = domEvent Touchstart canvasEl
      tupE    = domEvent Touchend canvasEl
      moveE  = domEvent Mousemove canvasEl
      downE  = domEvent Mousedown canvasEl
      upE    = domEvent Mouseup canvasEl
      pressedE = leftmost [Pressed <$ tdownE, Unpressed <$ upE]
      predrawE = leftmost [sqUpdE, (Clear,(0,0),emptySq) <$ upPrE]
      selE = fmap (\(dc, sqs) -> (dc, fmap fst sqs)) $ fmap (\(dc,sqs) -> (dc, filter jstFilter sqs)) $ fmap (\(dc,_,sqs) -> (dc,sqs)) predrawE
  tmovePrE <- performEvent $ ffor tmoveE prepTCoord
  tdownPrE <- performEvent $ ffor tdownE prepTCoord
  tupPrE   <- performEvent $ ffor tupE   prepTCoord
  movePrE  <- performEvent $ ffor moveE  prepCoord
  downPrE  <- performEvent $ ffor downE  prepCoord
  upPrE    <- performEvent $ ffor upE    prepCoord
  touchD <- holdDyn Unpressed pressedE
  sqUpdE <- performEvent $ ffor (leftmost [movePrE, downPrE]) $ \(x,y) -> do
    (sd :: (DrawCommand, (Double, Double))) <- sample . current $ moveD
    pure (AddSquare,(x,y),hitOrMiss (x,y) coords sd)

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

  performEvent_ $ ffor (leftmost [() <$ downE, () <$ upE, buildE]) $ \_ -> do
    dGridS <- sample . current $ dGridT
    rawJSCall (_element_raw canvasEl) dGridS

  performEvent_ $ ffor (leftmost [() <$ moveE, () <$ downE]) $ \_ -> do
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
      cOpts = CanvasOptions canvasW canvasH "pattern" "pattern"
      coords = zip [0..] $ colList canvasW canvasW 3
      emptySq = zip (take 9 (repeat Nothing)) $ colList canvasW canvasW 3

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

hitOrMiss :: Coursor -> [(Int, Square)] -> (DrawCommand, (Double, Double)) -> [(Maybe Int, Square)]
hitOrMiss (x,y) squares (dc,(oldX,oldY)) = fmap (\(num,(sqX, sqY, sqW, sqH)) -> if (((sqX-5) < x) && ((sqY-5) < y) && ((sqX + sqW + 5) > x) && ((sqY + sqH + 5) > y))
  then (Just num, (sqX, sqY, sqW, sqH))
  else case dc of
    AddSquare -> if (((sqX-5) < midX) && ((sqY-5) < midY) && ((sqX + sqW + 5) > midX) && ((sqY + sqH + 5) > midY))
      then (Just num, (sqX, sqY, sqW, sqH))
      else (Nothing, (sqX, sqY, sqW, sqH))
    Clear -> (Nothing, (sqX, sqY, sqW, sqH))
  ) squares
  where
    midX = if x > oldX
      then ((x - oldX)/2) + oldX
      else ((oldX - x)/2) + x
    midY = if y > oldY
      then ((y - oldY)/2) + oldY
      else ((oldY - y)/2) + y

colList :: Int -> Int -> Int -> [Square]
colList width height count = mconcat $ fmap (\num -> rowList width height count num) rList
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
