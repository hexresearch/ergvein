{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveTraversable         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
-- |
-- A subset of the <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D 2D Canvas API>.
--
-- These are experimental and do not cover even half of the API, it is likely
-- they will be wholly deprecated in the near future.
module Reflex.Dom.Canvas.Context2D where

import           Control.Monad.Free.Church          (F, foldF, liftF)

import           GHCJS.DOM.CanvasPath               as C
import           GHCJS.DOM.CanvasRenderingContext2D as C

import           GHCJS.DOM.Enums                    (CanvasWindingRule)
import           GHCJS.DOM.Types                    (JSString, MonadJSM)

-- Disallow because we want to control this externally
-- Create Int Int (Canvas -> a)
-- GetContext Canvas (Context -> a)
-- Width Canvas (Int -> a)
-- Height Canvas (Int -> a)
-- SetHeight Int Canvas a
-- SetWidth Int Canvas a

-- Disallow due to expense, for now. Has great use for preserving transforms
-- for child layers, given that the canvas is a giant global monster however.
-- That may just never work.
-- Save Context (Context -> a)
 -- Restore Context (Context -> a)

-- Isn't there...
-- CreatePattern Image Repeat (Pattern -> a)
-- Has the wrong type...
-- IsPointInPath Double Double (Bool -> a)

-- | Subset of allowable instructions for a 2D canvas context.
data CanvasF a
  = Transform Float Float Float Float Float Float a
  | Fill CanvasWindingRule a
  | FillRect Float Float Float Float a
  | BeginPath a
  | MoveTo Double Double a
  | LineTo Double Double a
  | ClosePath a
  | StrokeStyle JSString a
  | Stroke a
  | Clip CanvasWindingRule a

  | QuadraticCurveTo Double Double Double Double a
  | BezierCurveTo Double Double Double Double Double Double a
  | Arc Double Double Double Double Double Bool a
  | ArcTo Double Double Double Double Double a

  | Rect Double Double Double Double a
  | ClearRect Float Float Float Float a
  | StrokeRect Float Float Float Float a

  | Done a
  deriving (Functor, Foldable, Traversable, Show, Eq)

type CanvasM = F CanvasF

drawToCanvas
  :: MonadJSM m
  => F CanvasF a
  -> C.CanvasRenderingContext2D
  -> m a
drawToCanvas instructions cxt =
  foldF ( applyInstruction cxt ) instructions

applyInstruction :: MonadJSM m => C.CanvasRenderingContext2D -> CanvasF a -> m a
applyInstruction cxt instruction =
  case instruction of
    BeginPath cont             -> cont <$ C.beginPath cxt
    ClearRect x y w h cont     -> cont <$ C.clearRect cxt x y w h
    Clip rule cont             -> cont <$ C.clip cxt (Just rule)
    ClosePath cont             -> cont <$ C.closePath cxt
    Fill rule cont             -> cont <$ C.fill cxt ( Just rule )
    FillRect x y w h cont      -> cont <$ C.fillRect cxt x y w h
    LineTo x y cont            -> cont <$ C.lineTo cxt x y
    MoveTo x y cont            -> cont <$ C.moveTo cxt x y
    Rect x y w h cont          -> cont <$ C.rect cxt x y w h
    Stroke cont                -> cont <$ C.stroke cxt
    StrokeRect x y w h cont    -> cont <$ C.strokeRect cxt x y w h
    StrokeStyle style cont     -> cont <$ C.setStrokeStyle cxt style
    Transform a b c d e f cont -> cont <$ C.transform cxt a b c d e f

    Arc x y radius startAngle endAngle anticlockwise cont -> cont <$ C.arc cxt x y radius startAngle endAngle anticlockwise
    ArcTo cp1_X cp1_Y cp2_X cp2_Y radius cont             -> cont <$ C.arcTo cxt cp1_X cp1_Y cp2_X cp2_Y radius
    BezierCurveTo cp1_X cp1_Y cp2_X cp2_Y endX endY cont  -> cont <$ C.bezierCurveTo cxt cp1_X cp1_Y cp2_X cp2_Y endX endY
    QuadraticCurveTo cpX cpY endX endY cont               -> cont <$ C.quadraticCurveTo cxt cpX cpY endX endY

    -- DrawImage img dw dh cont                              -> cont <$ C.drawImage cxt img dw dh

    Done a                     -> pure a

  --  SetTransform Double Double Double Double Double Double a
  --  Scale Double Double a
  --  Translate Double Double a
  --  Rotate Double a
  --  FillRule JSString a
  --  FillStyle CanvasStyle a
  --  GlobalAlpha Double a
  --  LineJoin C.LineJoin a
  --  LineCap C.LineCap a
  --  MiterLimit Double a
  --  SetLineDash JSArray a
  --  LineDashOffset Double a
  --  TextAlign C.TextAlign a
  --  TextBaseline C.TextBaseline a
  --  LineWidth Double a
  --  Font JSString a
  --  MeasureText JSString (Double -> a)
  --  FillText JSString Double Double a
  --  StrokeText JSString Double Double a
  --  DrawImage CanvasImageSource Float Float a

    --  FillText text x y cont                                -> cont <$ C.fillText text x y
    --  Font font' cont                                       -> cont <$ C.font font'
    --  GlobalAlpha value cont                                -> cont <$ C.globalAlpha value
    --  LineCap linecap cont                                  -> cont <$ C.lineCap linecap
    --  LineDashOffset offset cont                            -> cont <$ C.lineDashOffset offset
    --  LineJoin linejoin cont                                -> cont <$ C.lineJoin linejoin
    --  LineWidth width cont                                  -> cont <$ C.lineWidth width
    --  MeasureText text cont                                 -> cont <$ C.measureText text
    --  MiterLimit limit cont                                 -> cont <$ C.miterLimit limit
    --  Rotate angle cont                                     -> cont <$ C.rotate angle
    --  Scale x y cont                                        -> cont <$ C.scale x y
    --  SetLineDash distances cont                            -> cont <$ C.setLineDash distances
    --  SetTransform a b c d e f cont                         -> cont <$ C.setTransform a b c d e f
    --  StrokeText text x y cont                              -> cont <$ C.strokeText text x y
    --  TextAlign alignment cont                              -> cont <$ C.textAlign alignment
    --  TextBaseline baseline cont                            -> cont <$ C.textBaseline baseline
    --  Translate x y cont                                    -> cont <$ C.translate x y
    --  DrawImage img dw dh cont                              -> cont <$ C.drawImage cxt img dw dh
    --  FillRule rule cont                                    -> cont <$ C.setFillRule cxt rule
    --  FillStyle style cont                                  -> cont <$ C.setFillStyle cxt style


fillF :: CanvasWindingRule -> CanvasM ()
fillF rule = liftF $ Fill rule ()

strokeF :: CanvasM ()
strokeF = liftF $ Stroke ()

strokeStyleF :: JSString -> CanvasM ()
strokeStyleF style = liftF $ StrokeStyle style ()

beginPathF :: CanvasM ()
beginPathF = liftF $ BeginPath ()

closePathF :: CanvasM ()
closePathF = liftF $ ClosePath ()

clipF :: CanvasWindingRule -> CanvasM ()
clipF rule = liftF $ Clip rule ()

rectF :: Double -> Double -> Double -> Double -> CanvasM ()
rectF x y h w = liftF $ Rect x y w h ()

doneF :: CanvasM ()
doneF = liftF $ Done ()

moveToF :: Double -> Double -> CanvasM ()
moveToF x y = liftF $ MoveTo x y ()

lineToF :: Double -> Double -> CanvasM ()
lineToF x y = liftF $ LineTo x y ()

clearRectF, fillRectF, strokeRectF :: Float -> Float -> Float -> Float -> CanvasM ()
clearRectF x y w h  = liftF $ ClearRect x y w h ()
fillRectF x y w h   = liftF $ FillRect x y w h ()
strokeRectF x y w h = liftF $ StrokeRect x y w h ()

quadraticCurveToF :: Double -> Double -> Double -> Double -> CanvasM ()
quadraticCurveToF cpX cpY endX endY = liftF $ QuadraticCurveTo cpX cpY endX endY ()

bezierCurveToF :: Double -> Double -> Double -> Double -> Double -> Double -> CanvasM ()
bezierCurveToF cp1_X cp1_Y cp2_X cp2_Y endX endY = liftF $ BezierCurveTo cp1_X cp1_Y cp2_X cp2_Y endX endY ()

arcF :: Double -> Double -> Double -> Double -> Double -> Bool -> CanvasM ()
arcF x y radius startAngle endAngle anticlockwise = liftF $ Arc x y radius startAngle endAngle anticlockwise ()

arcToF :: Double -> Double -> Double -> Double -> Double -> CanvasM ()
arcToF cp1_X cp1_Y cp2_X cp2_Y radius = liftF $ ArcTo cp1_X cp1_Y cp2_X cp2_Y radius ()
