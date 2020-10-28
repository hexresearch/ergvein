{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-all #-}
module Ergvein.Wallet.Page.PatternKey(
    patternAsk
  , patternAskWidget
  , patternSave
  , patternSaveWidget
  , portfolioWidget
  , loadCounter
  , saveCounter
  , PatternSavingTry(..)
  , PatternTries(..)
  ) where

import Control.Monad.IO.Class
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)

import Ergvein.Aeson
import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.PatternKey
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Canvas
import Ergvein.Wallet.Util

import qualified Data.Map.Strict as Map

data PatternTries = PatternTries {
  patterntriesCount  :: Map.Map Text Integer
} deriving (Eq, Show)

$(deriveJSON (aesonOptionsStripPrefix "pattern") ''PatternTries)

emptyPT :: PatternTries
emptyPT = PatternTries Map.empty
{-# INLINE emptyPT #-}

patternAsk :: MonadFrontBase t m => m (Dynamic t Password, Dynamic t TouchState)
patternAsk = divClass "pattern-container" $ mdo
  buildE <- delay 0.1 =<< getPostBuild
  canvasEl <- createCanvas cOpts
  let elP = elementPosition $ _element_raw canvasEl
      prepCoord (x,y) = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral x) - crLeft, (fromIntegral y) - crTop)) elP
      prepTCoord TouchEventResult{..} = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral (_touchResult_screenX (head _touchEventResult_touches))) - crLeft, (fromIntegral (_touchResult_screenY (head _touchEventResult_touches)) - crTop - 35))) elP
      tmoveE = domEvent Touchmove canvasEl
      tdownE  = domEvent Touchstart canvasEl
      tupE    = domEvent Touchend canvasEl
      pressedE = leftmost [Pressed <$ tdownE, Unpressed <$ tupE]
      predrawE = leftmost [squaresUpdatedE, (Clear,(0,0),emptySq) <$ tupPrE]
      selE = fmap (\(dc, sqs) -> (dc, fmap fst sqs)) $ fmap (\(dc,sqs) -> (dc, filter jstFilter sqs)) $ fmap (\(dc,_,sqs) -> (dc,sqs)) predrawE
      gridE = leftmost [() <$ tdownE, () <$ tupE, buildE]
      lineE = leftmost [() <$ tmoveE, () <$ tdownE]
  tmovePrE <- performEvent $ ffor tmoveE prepTCoord
  tdownPrE <- performEvent $ ffor tdownE prepTCoord
  tupPrE   <- performEvent $ ffor tupE   prepTCoord
  touchD <- holdDyn Unpressed pressedE

  squaresUpdatedE <- performEvent $ ffor (leftmost [tmovePrE, tdownPrE]) $ \(x,y) -> do
    sd <- sampleDyn moveD
    pure (AddSquare,(x,y),hitOrMiss (x,y) coords sd)
  -- Grid dynamic
  dGrid <- holdDyn (drawGridT canvasW canvasH emptySq) $ never
  -- Dynamic with squares list
  squaresD <- holdDyn (Clear,(0,0),emptySq) $ poke predrawE $ \(dc,cur,sqs) -> do
    touchS <- sampleDyn touchD
    case touchS of
      Pressed -> case dc of
        AddSquare -> do
          (_,_,sqsv) <- sampleDyn squaresD
          pure (AddSquare,cur,addToSquareList sqs sqsv)
        Clear -> pure (Clear,(0,0),emptySq)
      Unpressed -> pure (Clear,(0,0),emptySq)

  selectedD <- holdDyn (Clear,[]) $ poke selE $ \(dc, sqs) -> do
    touchS <- sampleDyn touchD
    case touchS of
      Pressed -> case dc of
        AddSquare -> if (null sqs)
          then do
            v <- sampleDyn selectedD
            pure v
          else do
            (dc,v) <- sampleDyn selectedD
            case (find (==(head sqs)) v) of
              Just _ -> pure (AddSquare, v)
              Nothing -> pure $ (AddSquare, v <> [head sqs])
        Clear -> pure (Clear, [])
      Unpressed -> pure (Clear, [])

  moveD <- holdDyn (Clear,(0,0)) $ fmap lastSquarePosition $ updated selectedD

  drawE <- performEvent $ ffor (updated squaresD) $ \a -> do
    sel <- sampleDyn selectedD
    ln <- sampleDyn moveD
    pure (a,sel,ln)

  dLineT <- holdDyn (drawLineZeroT) $ ffor drawE $ \((_,(x,y),r),sel,ln) -> (drawLineT canvasW canvasH x y 0 0 ln r)
                                                                          <> (drawLinesT sel coords)

  performEvent_ $ ffor gridE $ \_ -> do
    dGridS <- sampleDyn dGrid
    rawJSCall (_element_raw canvasEl) dGridS

  performEvent_ $ ffor lineE $ \_ -> do
    dLineS <- sampleDyn dLineT
    rawJSCall (_element_raw canvasEl) dLineS

  pure $ (fmap showt $ clearSelectionDynamic selectedD, touchD)
    where
      canvasH = 320
      canvasW = 320
      cOpts = CanvasOptions canvasW canvasH "pattern" "pattern"
      coords = zip [0..] $ colList canvasW canvasW 3

data PatternSavingTry = PatternSavingTry
  { firstTry  :: [Int]
  , secondTry :: [Int]
  } deriving (Show)

patternSave :: MonadFrontBase t m => Dynamic t PatternTry -> m (Dynamic t [Int], Dynamic t TouchState)
patternSave tryD = divClass "pattern-container" $ mdo
  buildE <- delay 0.1 =<< getPostBuild
  canvasEl <- createCanvas cOpts
  let elP = elementPosition $ _element_raw canvasEl
      prepCoord (x,y) = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral x) - crLeft, (fromIntegral y) - crTop)) elP
      prepTCoord TouchEventResult{..} = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral (_touchResult_screenX (head _touchEventResult_touches))) - crLeft, (fromIntegral (_touchResult_screenY (head _touchEventResult_touches)) - crTop - 35))) elP
      tmoveE = domEvent Touchmove canvasEl
      tdownE  = domEvent Touchstart canvasEl
      tupE    = domEvent Touchend canvasEl
      pressedE = leftmost [Pressed <$ tdownE, Unpressed <$ tupE]
      predrawE = leftmost [squaresUpdatedE, (Clear,(0,0),emptySq) <$ tupPrE]
      selE = fmap (\(dc, sqs) -> (dc, fmap fst sqs)) $ fmap (\(dc,sqs) -> (dc, filter jstFilter sqs)) $ fmap (\(dc,_,sqs) -> (dc,sqs)) predrawE
      gridE = leftmost [() <$ tdownE, () <$ tupE, buildE]
      lineE = leftmost [() <$ tmoveE, () <$ tdownE]
  tmovePrE <- performEvent $ ffor tmoveE prepTCoord
  tdownPrE <- performEvent $ ffor tdownE prepTCoord
  tupPrE   <- performEvent $ ffor tupE   prepTCoord
  touchD <- holdDyn Unpressed pressedE

  squaresUpdatedE <- performEvent $ ffor (leftmost [tmovePrE, tdownPrE]) $ \(x,y) -> do
    sd <- sampleDyn moveD
    pure (AddSquare,(x,y),hitOrMiss (x,y) coords sd)
  -- Grid dynamic
  dGrid <- holdDyn (drawGridT canvasW canvasH emptySq) $ never
  -- Dynamic with squares list
  squaresD <- holdDyn (Clear,(0,0),emptySq) $ poke predrawE $ \(dc,cur,sqs) -> do
    tryS<- sampleDyn tryD
    squaresS <- sampleDyn squaresD
    touchS <- sampleDyn touchD
    case tryS of
      Done -> pure squaresS
      _    ->  case touchS of
                Pressed -> case dc of
                  AddSquare -> do
                    (_,_,sqsv) <- sampleDyn squaresD
                    pure (AddSquare,cur,addToSquareList sqs sqsv)
                  Clear -> pure (Clear,(0,0),emptySq)
                Unpressed -> pure (Clear,(0,0),emptySq)

  selectedD <- holdDyn (Clear,[]) $ poke selE $ \(dc, sqs) -> do
    tryS<- sampleDyn tryD
    selectedS <- sampleDyn selectedD
    touchS <- sampleDyn touchD
    case tryS of
      Done -> pure selectedS
      _    -> case touchS of
                Pressed -> case dc of
                  AddSquare -> if (null sqs)
                    then do
                      v <- sampleDyn selectedD
                      pure v
                    else do
                      (dc,v) <- sampleDyn selectedD
                      case (find (==(head sqs)) v) of
                        Just _ -> pure (AddSquare, v)
                        Nothing -> pure $ (AddSquare, v <> [head sqs])
                  Clear -> pure (Clear, [])
                Unpressed -> pure (Clear, [])

  moveD <- holdDyn (Clear,(0,0)) $ fmap lastSquarePosition $ updated selectedD

  dlineE  <- performEvent $ ffor (updated squaresD) $ \(_,(x,y),r) -> do
    sel <- sampleDyn selectedD
    ln <- sampleDyn moveD
    pure $ (drawLineT canvasW canvasH x y 0 0 ln r) <> (drawLinesT sel coords)

  dLineT <- holdDyn (drawLineZeroT) $ poke dlineE $ \t -> do
    tryS <- sampleDyn tryD
    dLineS <- sampleDyn dLineT
    case tryS of
      Done -> pure dLineS
      _    -> pure t
  let drawKeyCreation = do
        tryS <- sampleDyn tryD
        performEvent_ $ ffor gridE $ \_ -> do
          dGridS <- sampleDyn dGrid
          rawJSCall (_element_raw canvasEl) dGridS

        performEvent_ $ ffor lineE $ \_ -> do
          dLineS <- sampleDyn dLineT
          rawJSCall (_element_raw canvasEl) dLineS

  widgetHold (drawKeyCreation) $ ffor (updated tryD) $ \a -> case a of
    Done -> do
      dLineS <- sampleDyn dLineT
      rawJSCall (_element_raw canvasEl) dLineS
    _ -> drawKeyCreation

  pure $ (clearSelectionDynamic selectedD, touchD)
    where
      canvasH = 320
      canvasW = 320
      cOpts = CanvasOptions canvasW canvasH "pattern" "pattern"
      coords = zip [0..] $ colList canvasW canvasW 3

patternAskWidget :: MonadFrontBase t m => m (Dynamic t Password)
patternAskWidget = mdo
  patternD <- holdDyn "" patternE
  (pD, touchD) <- patternAsk
  patternE <- performEvent $ ffor (ffilter (\e -> e == Unpressed) (updated touchD)) $ \_ -> do
    p <- sampleDyn pD
    pure p
  pure patternD

patternSaveWidget :: MonadFrontBase t m => m (Dynamic t Password)
patternSaveWidget = mdo
  widgetHold (localizedText PKSFirstTry) $ ffor (updated tryD) $ \a -> case a of
    FirstTry -> localizedText PKSFirstTry
    SecondTry -> localizedText PKSSecondTry
    ErrorTry -> localizedText PKSErrorTry
    Done -> localizedText PKSDone
  tryD <- holdDyn FirstTry tryE
  (listD, touchD) <- patternSave tryD
  patternD <- holdDyn (PatternSavingTry [] []) patternE
  tryE <- performEvent $ ffor (updated touchD) $ \press -> do
    tryS <- sampleDyn tryD
    case press of
       Unpressed -> case tryS of
         FirstTry -> pure SecondTry
         SecondTry -> do
           PatternSavingTry{..} <- sampleDyn patternD
           if firstTry == secondTry
             then pure Done
             else pure ErrorTry
         ErrorTry -> pure SecondTry
         Done -> pure Done
       Pressed -> pure tryS
  patternE <- performEvent $ ffor (updated listD) $ \a -> do
    patternS <- sampleDyn patternD
    tryS <- sampleDyn tryD
    case tryS of
      FirstTry -> pure $ PatternSavingTry a []
      ErrorTry -> pure $ PatternSavingTry a []
      SecondTry -> pure $ PatternSavingTry (firstTry patternS) a
      Done -> pure patternS
  passOkE <- performEvent $ ffor (updated patternD) $ \PatternSavingTry{..} -> do
    tryS <- sampleDyn tryD
    case tryS of
      Done -> pure $ showt firstTry
      _ -> pure ""
  passD <- holdDyn "" passOkE
  pure passD

saveCounter :: (MonadIO m, PlatformNatives, HasStoreDir m) => PatternTries -> m ()
saveCounter pt = storeValue "tries.json" pt True

loadCounter :: (MonadIO m, PlatformNatives, HasStoreDir m) => m PatternTries
loadCounter = fmap (fromRight emptyPT) $ retrieveValue "tries" emptyPT

clearSelectionDynamic :: Reflex t => Dynamic t (DrawCommand, [Maybe Int]) -> Dynamic t [Int]
clearSelectionDynamic selD = fmap (fmap (\a -> fromMaybe 0 a)) $ fmap snd selD

jstFilter :: (Maybe Int, a) -> Bool
jstFilter a = case (fst a) of
  Just n -> True
  Nothing -> False

addToSquareList :: [(Maybe Int, Square)] -> [(Maybe Int, Square)] -> [(Maybe Int, Square)]
addToSquareList a b = (\((mi1,f1),(mi2,f2)) -> case mi1 of
  Just n1 -> (Just n1, f1)
  Nothing -> case mi2 of
    Just n2 -> (Just n2, f2)
    Nothing -> (Nothing,f2)
  ) <$> (zip a b)

hitOrMiss :: Position -> [(Int, Square)] -> (DrawCommand, Position) -> [(Maybe Int, Square)]
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

emptySq :: [(Maybe Int, Square)]
emptySq = zip (take 9 (repeat Nothing)) $ colList 320 320 3

lastSquareCenter :: [Maybe Int] -> Position
lastSquareCenter lst = case (last lst) of
  Just num -> (\(a,b,c,d) -> (a+c/2,b+d/2)) $ snd $ emptySq !! num
  Nothing -> (0,0)

lastSquarePosition :: (DrawCommand,[Maybe Int]) -> (DrawCommand, Position)
lastSquarePosition (Clear, _) = (Clear,(0,0))
lastSquarePosition (AddSquare,lst) = if null lst
  then (AddSquare,(0,0))
  else (AddSquare,lastSquareCenter lst)


portfolioWidget :: MonadFrontBase t m => m ()
portfolioWidget = divClass "portfolio-idget" $ do
  divClass "canvas-container" $ mdo
    buildE <- delay 0.1 =<< getPostBuild
    canvasEl <- createCanvas cOpts
    let elP = elementPosition $ _element_raw canvasEl
        prepCoord (x,y) = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral x) - crLeft, (fromIntegral y) - crTop)) elP
        prepTCoord TouchEventResult{..} = fmap (\(a,b)-> (a, b)) $ fmap (\ClientRect{..} -> ((fromIntegral (_touchResult_screenX (head _touchEventResult_touches))) - crLeft, (fromIntegral (_touchResult_screenY (head _touchEventResult_touches)) - crTop - 35))) elP
        tmoveE = domEvent Touchmove canvasEl
        tdownE = domEvent Touchstart canvasEl
        tupE   = domEvent Touchend canvasEl
        mmoveE = domEvent Mousemove canvasEl
        pressedE = leftmost [Pressed <$ tdownE, Unpressed <$ tupE]
    tmovePrE <- performEvent $ ffor tmoveE prepTCoord
    tdownPrE <- performEvent $ ffor tdownE prepTCoord
    tupPrE   <- performEvent $ ffor tupE   prepTCoord
    mmovePrE <- performEvent $ ffor mmoveE prepCoord
    touchD <- holdDyn Unpressed pressedE
    ringD <- holdDyn [(0.6,Unhovered),(0.4,Unhovered)] $ poke mmovePrE $ \(x,y) -> do
      ringS <- sampleDyn ringD
      pure $ checkRingHover (x,y) (dW,dH) dr ringS
    performEvent_ $ ffor (updated ringD) $ \lst -> do
      rawJSCall (_element_raw canvasEl) $ drawRndHovT canvasW canvasH lst
    performEvent_ $ ffor buildE $ \_ -> do
      rawJSCall (_element_raw canvasEl) $ drawRndHovT canvasW canvasH [(0.6,Unhovered),(0.4,Unhovered)]
    pure $ ()
  divClass "portfolio-legend" $ do
    pure ()
      where
        canvasH = 240
        canvasW = 240
        dH = fromIntegral canvasH
        dW = fromIntegral canvasW
        dr = (dH/2)*0.98
        cOpts = CanvasOptions canvasW canvasH "pattern" "pattern"


checkRingHover :: (Double, Double) -> (Double,Double) -> Double -> [(Double,HoverState)] -> [(Double,HoverState)]
checkRingHover (x,y) (w,h) r lst = if ringHitOrMiss
    then arcHitOrMiss angle
    else fmap (\(a,_) -> (a,Unhovered)) lst
  where
    ringHitOrMiss = (frml <= rs*rs) && (frml >= (rs/2)*(rs/2))
    frml = (x - rs)*(x - rs) + (y - rs)*(y - rs)
    anglePre = (pi) - atan2 (x - rs) (y - rs)
    angle = if anglePre < 0
      then anglePre + 2*pi
      else anglePre
    arcHitOrMiss ang = fmap (\(a,(st,end)) -> if ((ang > st) && (ang< end))
      then (a,Hovered)
      else (a,Unhovered)
       ) $ zip lunh lok
    tp = -pi/2
    crcl = pi*2
    rs = r/0.98
    lunh = fmap fst lst
    l3 = scanl (\x y -> x+y*2*pi) 0 lunh
    lok = zip (init l3) (tail l3)
