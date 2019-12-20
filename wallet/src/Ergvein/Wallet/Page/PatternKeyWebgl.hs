module Ergvein.Wallet.Page.PatternKeyWebgl(
    patternKeyWidgetWebgl
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
import           Data.Time                        (UTCTime, getCurrentTime)
import qualified Ergvein.Wallet.Page.PatternKeyUtils.Utils as UT

import qualified GHCJS.DOM.Types                     as Dom
import qualified Language.Javascript.JSaddle.Object  as JSO
import qualified GHCJS.DOM.WebGLRenderingContextBase as Gl


patternKeyWidgetWebgl :: MonadFrontBase t m => m ()
patternKeyWidgetWebgl = divClass "myTestDiv" $ do
  text $ "======================================"
{-  let
    canvasH = 480
    canvasW = 640
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

  canvasEl <- fst <$> RD.elAttr' "canvas" canvasAttrs RD.blank

  d2D <- fmap (^. Canvas.canvasInfo_context) <$> CDyn.dContext2d ( Canvas.CanvasConfig . [] )

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
      traverse_ UT.lineInstruction xs
      CanvasF.closePathF
      CanvasF.strokeStyleF "#000000"
      CanvasF.strokeF

    dLines = ( ^. UT.dataSet_lines . to toCM )
      <$> dDataLines

  _ <- CDyn.nextFrameWithCxFree dLines d2D eTicken

  divClass "myDebugLog" $ text $ showt dDataLines

  text $ "======================================"
  pure ()


vertShader :: Text
vertShader = T.unlines
  [ "attribute vec4 a_position;"
  , "void main() {"
  , "  gl_Position = a_position;"
  , "}"
  ]

fragShader :: Text
fragShader = T.unlines
  [ "precision mediump float;"
  , "void main() {"
  , "  gl_FragColor = vec4(1, 0, 0.5, 1);"
  , "}"
  ]

positions :: [Double]
positions =
  [ 0.0, 0.0
  , 0.0, 0.5
  , 0.7, 0.0
  ]

makeArrayBuffer :: MonadJSM m => [Double] -> m Dom.ArrayBuffer
makeArrayBuffer ds = Dom.liftJSM $
  JSO.new (JSO.jsg ("Float32Array" :: Text)) [ds]
  >>= (JSO.! ("buffer" :: Text))
  >>= Dom.unsafeCastTo Dom.ArrayBuffer

data RenderMeh = R
  { _rGLProgram  :: Dom.WebGLProgram
  , _rPosAttrLoc :: Dom.GLint
  , _rPosBuffer  :: Dom.WebGLBuffer
  }

glProgramInit :: Text -> Text -> Gl.WebGLM (Either JSString RenderMeh)
glProgramInit vertSrc fragSrc = runExceptT $ do
  -- Begin initialisation
  vS     <- ExceptT $ Gl.buildShader vertSrc Gl.VERTEX_SHADER
  fS     <- ExceptT $ Gl.buildShader fragSrc Gl.FRAGMENT_SHADER
  glProg <- ExceptT $ Gl.buildProgram vS fS

  -- Buffer Setup and Loading
  posAttrLoc <- lift $ Gl.getAttribLocationF glProg ( "a_position" :: Text )
  lift $ Gl.enableVertexAttribArrayF (fromIntegral posAttrLoc)

  posBuffer <- lift Gl.createBufferF

  pure $ R glProg posAttrLoc posBuffer
-}
