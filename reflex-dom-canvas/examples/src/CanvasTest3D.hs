{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
--
module CanvasTest3D where

import           Control.Lens                        ((^.))

import qualified Reflex.Dom.Canvas.WebGL             as Gl

import           GHCJS.DOM.Types                     (JSString, MonadJSM)

import qualified GHCJS.DOM.Types                     as Dom

import qualified Language.Javascript.JSaddle.Object  as JSO

import qualified GHCJS.DOM.WebGLRenderingContextBase as Gl

import qualified Reflex.Dom.CanvasBuilder.Types      as Canvas
import qualified Reflex.Dom.CanvasDyn                as CDyn

import qualified Reflex                              as R

import           Reflex.Dom                          (MonadWidget)
import qualified Reflex.Dom                          as RD

import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Time                           (UTCTime, getCurrentTime)

import           Control.Monad.Except                (ExceptT (..), lift,
                                                      runExceptT)
import           Data.Either                         (Either)
import qualified Data.Map                            as Map

#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp    (run)
import           Reflex.Dom.Core                     (mainWidget)
#endif

vertShader
  :: Text
vertShader = Text.unlines
  [ "attribute vec4 a_position;"
  , "void main() {"
  , "  gl_Position = a_position;"
  , "}"
  ]

fragShader
  :: Text
fragShader = Text.unlines
  [ "precision mediump float;"
  , "void main() {"
  , "  gl_FragColor = vec4(1, 0, 0.5, 1);"
  , "}"
  ]

positions
  :: [Double]
positions =
  [ 0.0, 0.0
  , 0.0, 0.5
  , 0.7, 0.0
  ]

makeArrayBuffer
  :: MonadJSM m
  => [Double]
  -> m Dom.ArrayBuffer
makeArrayBuffer ds = Dom.liftJSM $
  JSO.new (JSO.jsg ("Float32Array" :: Text)) [ds]
  >>= (JSO.! ("buffer" :: Text))
  >>= Dom.unsafeCastTo Dom.ArrayBuffer

data RenderMeh = R
  { _rGLProgram  :: Dom.WebGLProgram
  , _rPosAttrLoc :: Dom.GLint
  , _rPosBuffer  :: Dom.WebGLBuffer
  }

glProgramInit
  :: Text
  -> Text
  -> Gl.WebGLM (Either JSString RenderMeh)
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

glDraw
  :: Dom.ArrayBuffer
  -> RenderMeh
  -> Gl.WebGLM ()
glDraw arrBuff R {..} = do
  -- Populate our buffer with some data
  Gl.bindBufferF Gl.ARRAY_BUFFER _rPosBuffer
  Gl.bufferDataF Gl.ARRAY_BUFFER arrBuff Gl.STATIC_DRAW

 -- Clear canvas
  Gl.clearColourF 0 0 0 0
  Gl.clearF Gl.COLOR_BUFFER_BIT

  -- Tell WebGL to use our prepared GLProgram
  Gl.useProgramF _rGLProgram

  let
    size      = 2               -- 2 components per iteration
    dataType  = Gl.FLOAT        -- the data is 32bit floats
    normalise = False           -- don't normalize the data
    stride    = 0        -- 0 for tightly packed array or move forward size * sizeof(type) each iteration to get the next position
    offset    = 0        -- start at the beginning of the buffer

  -- Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
  Gl.vertexAttribPointerF
    (fromIntegral _rPosAttrLoc)
    size
    dataType
    normalise
    stride
    offset

  let
    primitiveType = Gl.TRIANGLES
    count = 3
    offset' = 0

  Gl.drawArraysF primitiveType offset' count

eDraw :: MonadWidget t m => UTCTime -> m ()
eDraw _aTime = do
  let
    canvasId = "canvas-three-dee"
    canvasAttrs = pure $ Map.fromList
      [ ("height", "400")
      , ("width", "400")
      ]

  eInit <- RD.button "Init"
  eRender <- RD.button "Render"

  arrBuffer <- makeArrayBuffer positions

  -- Create the canvas element
  canvasEl <- fst <$> RD.elDynAttr' "canvas"
    (Map.insert "id" canvasId <$> canvasAttrs) RD.blank

  dGLCX <- fmap (^. Canvas.canvasInfo_context)
    <$> CDyn.dContextWebgl ( Canvas.CanvasConfig canvasEl [] )

  let
    dInitProg =
      pure $ glProgramInit vertShader fragShader

  (eInitFailed, eRenderMeh) <-
    R.fanEither <$> CDyn.nextFrameWithCxFree dInitProg dGLCX eInit

  dInstructions <- R.holdDyn Gl.noopF ( glDraw arrBuffer <$> eRenderMeh )

  _ <- CDyn.nextFrameWithCxFree dInstructions dGLCX eRender

  dStatus <- R.holdDyn "A little nothing..." eInitFailed

  RD.divClass "errorz" $
    RD.display dStatus

  pure ()

mainish
  :: IO ()
mainish = do
  n <- getCurrentTime
#ifdef ghcjs_HOST_OS
  RD.mainWidget ( eDraw n )
#else
  run 8080 $ mainWidget ( eDraw n )
#endif
