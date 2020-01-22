{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Main functions for creating your Dynamic canvas element
module Reflex.Dom.CanvasDyn
  (
  -- * Create Canvas Context
  --
  -- |
  -- Create a <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D 2D> or <https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext WebGL> context from the given 'CanvasConfig'.

    dContext2d
  , dContextWebgl

  -- * Apply Canvas Updates
  --
  -- |
  -- These functions apply the function to the
  -- 'Reflex.Dom.CanvasBuilder.Types.RenderContext' and then pass it to
  -- 'JSDOM.nextAnimationFrame' to apply the changes to the canvas.

  -- ** With @tagPromptlyDyn@
  , nextFrameWithCxPromplyDyn
  , nextFrameWithCxFreePromptlyDyn

  -- ** With @tag@
  , nextFrameWithCx
  , nextFrameAsyncWithCx
  , nextFrameWithCxFree
 ) where

import           Control.Lens                   ((^.))

import           Data.Coerce                    (coerce)
import           Data.Proxy                     (Proxy (..))
import           GHC.TypeLits                   (KnownSymbol, symbolVal)

import qualified GHCJS.DOM                      as JSDOM
import           GHCJS.DOM.HTMLCanvasElement    (getContextUnchecked)
import           GHCJS.DOM.Types                (IsRenderingContext, JSM,
                                                 MonadJSM,
                                                 RenderingContext (..),
                                                 fromJSValUnchecked, liftJSM,
                                                 toJSVal)

import           Reflex                         (Dynamic, Event, Performable)
import qualified Reflex                         as R

import           Reflex.Dom.Core                (MonadWidget)
import qualified Reflex.Dom.Core                as RD

import           Reflex.Dom.CanvasBuilder.Types

dCanvasCx
  :: forall c t m. ( MonadWidget t m
                   , KnownSymbol (RenderContextEnum c)
                   , IsRenderingContext (RenderContext c)
                   , HasRenderFn c (RenderContext c)
                   )
  => CanvasConfig c t
  -> m (Dynamic t (CanvasInfo c t))
dCanvasCx cfg = do
  let
    reflexEl = cfg ^. canvasConfig_El
    cxType   = symbolVal (Proxy :: Proxy (RenderContextEnum c))

  renderCx <- liftJSM $ do
    e  <- fromJSValUnchecked =<< toJSVal (RD._element_raw reflexEl)
    getContextUnchecked e cxType (cfg ^. canvasConfig_Args)

  return . pure $ CanvasInfo reflexEl ( coerce renderCx) (`RD.keypress` reflexEl)

-- |
-- Create a canvas for 2D drawing using "context2d".
dContext2d
  :: MonadWidget t m
  => CanvasConfig 'TwoD t
  -> m (Dynamic t (CanvasInfo 'TwoD t))
dContext2d = dCanvasCx

-- |
-- Create a canvas for use with WebGL.
dContextWebgl
  :: MonadWidget t m
  => CanvasConfig 'Webgl t
  -> m (Dynamic t ( CanvasInfo 'Webgl t))
dContextWebgl = dCanvasCx

applyCanvasWithCx
  :: ( MonadWidget t m
     , MonadJSM jsm
     , HasRenderFn c (RenderContext c)
    )
  => (Dynamic t (jsm a) -> Event t () -> Event t (Performable m a))
  -> Dynamic t (RenderContext c)
  -> Dynamic t (RenderContext c -> Double -> JSM a)
  -> Event t ()
  -> m (Event t a)
applyCanvasWithCx tagFn dContext dAction eApply =
  let
    nextFrame cx f = liftJSM $
      JSDOM.nextAnimationFrame (f cx)
  in
    RD.performEvent (tagFn (nextFrame <$> dContext <*> dAction) eApply)

applyCanvasAsyncWithCx
  :: forall t m a c. 
    ( MonadWidget t m
    , RD.DomRenderHook t m
    , HasRenderFn c (RenderContext c)
    )
  => (Dynamic t (JSM a) -> Event t () -> Event t (JSM a))
  -> Dynamic t (RenderContext c)
  -> Dynamic t (RenderContext c -> JSM a)
  -> Event t ()
  -> m (Event t a)
applyCanvasAsyncWithCx tagFn dContext dAction eApply =
  RD.requestDomAction (tagFn (dAction <*> dContext) eApply)

applyCanvasFree
  :: ( MonadWidget t m
     , HasRenderFn c (RenderContext c)
     )
  => (Dynamic t (RenderContext c) -> Dynamic t (RenderContext c -> Double -> JSM a) -> Event t () -> m (Event t a))
  -> Dynamic t (RenderFree c a)
  -> Dynamic t (RenderContext c)
  -> Event t ()
  -> m (Event t a)
applyCanvasFree f dInstructions dContext =
  f dContext ((\i cx _ -> renderFunction cx i) <$> dInstructions)

-- |
-- These functions have the same warning as 'Reflex.Dynamic.tagPromptlyDyn' in that you
-- should not use the resulting 'Event' to update either of the 'Dynamic' input
-- values due to how the sampling is achieved with 'Reflex.Dynamic.tagPromptlyDyn'.
nextFrameWithCxPromplyDyn
  :: ( MonadWidget t m
     , HasRenderFn c (RenderContext c)
     )
  => Dynamic t (RenderContext c)
  -> Dynamic t (RenderContext c -> Double -> JSM a)
  -> Event t ()
  -> m ( Event t a)
nextFrameWithCxPromplyDyn =
  applyCanvasWithCx RD.tagPromptlyDyn

-- |
-- Evaluate a 'Reflex.Dom.CanvasBuilder.Types.WebGLM' or 'Reflex.Dom.CanvasBuilder.Types.CanvasM' using the given context.
nextFrameWithCxFreePromptlyDyn
  :: ( MonadWidget t m
     , HasRenderFn c (RenderContext c)
     )
  => Dynamic t (RenderFree c a)
  -> Dynamic t (RenderContext c)
  -> Event t ()
  -> m (Event t a)
nextFrameWithCxFreePromptlyDyn =
  applyCanvasFree nextFrameWithCxPromplyDyn

-- |
-- Perform the updates using 'Reflex.Class.tag' using the previous value of the 'Reflex.Class.Dynamic'.
nextFrameWithCx
  :: ( MonadWidget t m
     , HasRenderFn c (RenderContext c)
     )
  => Dynamic t (RenderContext c)
  -> Dynamic t (RenderContext c -> Double -> JSM a)
  -> Event t ()
  -> m ( Event t a)
nextFrameWithCx =
  applyCanvasWithCx (R.tag . R.current)

-- |
-- Perform the updates using 'Reflex.Class.tag' using the previous value of the 'Reflex.Class.Dynamic'.
nextFrameAsyncWithCx
  :: ( MonadWidget t m
     , RD.DomRenderHook t m
     , HasRenderFn c (RenderContext c)
     )
  => Dynamic t (RenderContext c)
  -> Dynamic t (RenderContext c -> JSM a)
  -> Event t ()
  -> m (Event t a)
nextFrameAsyncWithCx =
  applyCanvasAsyncWithCx (R.tag . R.current)

-- |
-- Evaluate a 'Reflex.Dom.CanvasBuilder.Types.WebGLM' or 'Reflex.Dom.CanvasBuilder.Types.CanvasM' using the given context.
nextFrameWithCxFree
  :: ( MonadWidget t m
     , HasRenderFn c (RenderContext c)
     )
  => Dynamic t (RenderFree c a)
  -> Dynamic t (RenderContext c)
  -> Event t ()
  -> m (Event t a)
nextFrameWithCxFree =
  applyCanvasFree nextFrameWithCx
