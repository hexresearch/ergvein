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
  text $ "======================================"
  let
    canvasH = 320
    canvasW = 320
    dataN   = 20

    canvasAttrs = Map.fromList
      [ ("height", T.pack . show $ canvasH)
      , ("width" , T.pack . show $ canvasW)
      , ("id"    , "fruity")
      ]

  aTime  <- liftIO $ getCurrentTime
  stdGen <- liftIO $ Rnd.getStdGen

  eStart <- RD.button "Start"
  eStop  <- RD.button "Stop"

  (canvasEl, _) <- RD.elAttr' "canvas" canvasAttrs RD.blank
  let moveE = domEvent Mousemove canvasEl
  let downE = domEvent Mousedown canvasEl
  let upE = domEvent Mouseup canvasEl
  d2D <- fmap (^. Canvas.canvasInfo_context) <$> CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )

  eTick <- RD.tickLossy 0.016 aTime

  eTicken <- fmap R.switch . R.hold R.never $ R.leftmost
    [ ()      <$ eTick <$ eStart
    , R.never <$ eStop
    ]

  dFloatFeed' <- UT.dFloatFeed ( 0.0, 450.0 ) stdGen eTicken

  dDataLines <- UT.dDataz canvasH canvasW dataN
    $ R.current dFloatFeed' <@ eTicken


  let
    toCM xs = do
      CanvasF.clearRectF 0.0 0.0 (fromIntegral canvasW) (fromIntegral canvasH)
      CanvasF.beginPathF
      CanvasF.rectF 0 0 (fromIntegral canvasW) (fromIntegral canvasH)
      traverse_ (\(a,b,c,d) -> CanvasF.rectF a b c d) $ reqList canvasW canvasW 3
--      traverse_ UT.lineInstruction xs
      CanvasF.closePathF
      CanvasF.strokeStyleF "#000000"
      CanvasF.strokeF

    dLines = ( ^. UT.dataSet_lines . to toCM )
      <$> dDataLines

  _ <- CDyn.nextFrameWithCxFree dLines d2D eTicken
  sizeE <- performEvent $ ffor moveE $ const $ do
     elementPosition (_element_raw canvasEl)
  --divClass "myDebugLog" $ dynText $ fmap showt dDataLines
  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor sizeE $ \tR -> do
      text $ showt $ tR
  --divClass "myDebugLog" $ dynText $ fmap showt dDataLines
  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor upE $ \tR -> do
      text $ showt $ tR
  --divClass "myDebugLog" $ dynText $ fmap showt dDataLines
  divClass "myDebugLog" $ widgetHold (text "empty") $ ffor downE $ \tR -> do
      text $ showt $ tR
  --text $ showt $ reqList canvasW canvasH 4


  text $ "======================================"
  pure ()

reqList :: Int -> Int -> Int -> [(Double, Double, Double, Double)]
reqList width height count = mconcat $ fmap (\num -> rowList width height count num) rList
  where
    stepW = fromIntegral $ floor (rW / (2*rCount+1))
    stepH = fromIntegral $ floor (rH / (2*rCount+1))
    rList = fmap fromIntegral $ [0 .. (count-1)]
    rCount = fromIntegral (count - 1)
    rW = fromIntegral width
    rH = fromIntegral height

rowList :: Int -> Int -> Int -> Double -> [(Double, Double, Double, Double)]
rowList width height count globN = fmap (\num -> (stepH*2*num + stepH,stepW*2*globN + stepW, stepW,stepH)) rList
  where
    stepW = fromIntegral $ floor (rW / (4*rCount+1))
    stepH = fromIntegral $ floor (rH / (4*rCount+1))
    rList = fmap fromIntegral $ [0 .. (count-1)]
    rCount = fromIntegral (count - 1)
    rW = fromIntegral width
    rH = fromIntegral height

data ClientRect = ClientRect {
    crBottom :: !Int
  , crHeight :: !Int
  , crLeft   :: !Int
  , crRight  :: !Int
  , crTop    :: !Int
  , crWidth  :: !Int
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
