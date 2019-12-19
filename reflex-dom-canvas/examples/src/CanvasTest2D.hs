{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module CanvasTest2D (mainish) where

import           Control.Lens                     (to, (^.))

import           Reflex                           ((<@))
import           Reflex.Dom                       (MonadWidget)

import qualified Reflex                           as R
import qualified Reflex.Dom                       as RD

import qualified Data.Text                        as Text

import           Data.Foldable                    (traverse_)

import qualified Reflex.Dom.Canvas.Context2D      as CanvasF

import qualified Reflex.Dom.CanvasBuilder.Types   as Canvas
import qualified Reflex.Dom.CanvasDyn             as CDyn

import           Data.Time                        (UTCTime, getCurrentTime)

import           System.Random                    (StdGen)
import qualified System.Random                    as Rnd

import qualified Data.Map                         as Map

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp (run)
import           Reflex.Dom.Core                  (mainWidget)
-- import qualified Run
#endif

import qualified Utils.ArbitraryDataFeed          as AF

eDraw
  :: MonadWidget t m
  => UTCTime
  -> StdGen
  -> m ()
eDraw aTime stdGen = do
  let
    canvasH = 480
    canvasW = 640
    dataN   = 20

    canvasAttrs = Map.fromList
      [ ("height", Text.pack . show $ canvasH)
      , ("width" , Text.pack . show $ canvasW)
      , ("id"    , "fruity")
      ]

  eStart <- RD.button "Start"
  eStop  <- RD.button "Stop"

  -- Create the canvas element
  canvasEl <- fst <$> RD.elAttr' "canvas" canvasAttrs RD.blank

  -- Create our canvas painter, will be restricted to 'context2d' because of the types! :D
  d2D <- fmap (^. Canvas.canvasInfo_context)
    <$> CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )

  eTick <- RD.tickLossy 0.016 aTime

  eTicken <- fmap R.switch . R.hold R.never $ R.leftmost
    [ ()      <$ eTick <$ eStart
    , R.never <$ eStop
    ]

  dFloatFeed' <- AF.dFloatFeed ( 0.0, 450.0 ) stdGen eTicken

  dDataLines <- AF.dDataz canvasH canvasW dataN
    $ R.current dFloatFeed' <@ eTicken

  let
    toCM xs = do
      CanvasF.clearRectF 0.0 0.0 (fromIntegral canvasW) (fromIntegral canvasH)
      CanvasF.beginPathF
      traverse_ AF.lineInstruction xs
      CanvasF.closePathF
      CanvasF.strokeStyleF "#000000"
      CanvasF.strokeF

    dLines = ( ^. AF.dataSet_lines . to toCM )
      <$> dDataLines

  _ <- CDyn.nextFrameWithCxFree dLines d2D eTicken

  RD.el "div" $
    RD.dynText ( ( Text.pack . show ) <$> dDataLines )

#ifdef ghcjs_HOST_OS
mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
  g <- Rnd.getStdGen
  RD.mainWidget ( eDraw n g )
#endif

#ifndef ghcjs_HOST_OS
mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
  g <- Rnd.getStdGen
  run 8080 $ mainWidget ( eDraw n g )
#endif
