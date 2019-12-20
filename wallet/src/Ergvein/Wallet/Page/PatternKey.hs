module Ergvein.Wallet.Page.PatternKey(
    patternKeyWidget
  ) where

import Ergvein.Crypto.Keys     (Mnemonic)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Storage.Data
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Storage.AuthInfo
import Reflex.Localize

import Reflex.Dom

import qualified Reflex.Dom                       as RD
import qualified Reflex.Dom.CanvasBuilder.Types   as Canvas
import qualified Reflex.Dom.CanvasDyn             as CDyn

import           Control.Lens                     (to, (^.))
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time                        (UTCTime, getCurrentTime)

import qualified Utils.ArbitraryDataFeed          as AF



patternKeyWidget :: MonadFrontBase t m => m ()
patternKeyWidget = divClass "myTestDiv" $ do
  text $ "======================================"
  let
    canvasH = 480
    canvasW = 640
    dataN   = 20

    canvasAttrs = Map.fromList
      [ ("height", T.pack . show $ canvasH)
      , ("width" , T.pack . show $ canvasW)
      , ("id"    , "fruity")
      ]


  eStart <- RD.button "Start"
  eStop  <- RD.button "Stop"

  canvasEl <- fst <$> RD.elAttr' "canvas" canvasAttrs RD.blank

  d2D <- fmap (^. Canvas.canvasInfo_context) <$> CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )

  text $ "======================================"
  pure ()


eDraw :: MonadWidget t m => UTCTime -> StdGen -> m ()
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
