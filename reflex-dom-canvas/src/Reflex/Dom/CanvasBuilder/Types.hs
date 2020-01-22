{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
--
{-# LANGUAGE FunctionalDependencies #-}
-- | Core types, typeclasses, and structures for handling the canvas contexts.
module Reflex.Dom.CanvasBuilder.Types where

import           Control.Lens                       (makeLenses)

import           GHC.TypeLits                       (Symbol)

import qualified Reflex                             as R
import qualified Reflex.Dom.Core                    as RD

import           GHCJS.DOM.CanvasRenderingContext2D (CanvasRenderingContext2D (..))

import           GHCJS.DOM.Types                    (IsRenderingContext, JSM,
                                                     WebGLRenderingContext)

import           Data.Text                          (Text)

import           Reflex.Dom.Canvas.WebGL            (WebGLM)
import qualified Reflex.Dom.Canvas.WebGL            as GL

import           Reflex.Dom.Canvas.Context2D        (CanvasM)
import qualified Reflex.Dom.Canvas.Context2D        as TwoD

-- | The basic canvas context types
data ContextType
  = TwoD
  | Webgl

-- | Type family to indicate the relationship between a ContextType and the
-- JSDOM type of the context object.
type family RenderContext (a :: ContextType) :: *
type instance RenderContext 'TwoD  = CanvasRenderingContext2D
type instance RenderContext 'Webgl = WebGLRenderingContext

-- | To create the context object, the JavaScript expects a stringly parameter.
-- This type family creates a type safe relationship between the type of context
-- you are requesting and the stringly input to the JavaScript function.
type family RenderContextEnum (a :: ContextType) :: Symbol
type instance RenderContextEnum 'TwoD  = "2d"
type instance RenderContextEnum 'Webgl = "webgl"

-- | Type family to connect a ContextType to the Free instructions for working
-- with a particular type of context. So you cannot run 2d drawing actions when
-- using a WebGL context.
type family RenderFree (a :: ContextType) :: * -> *
type instance RenderFree 'TwoD  = CanvasM
type instance RenderFree 'Webgl = WebGLM

-- | Configuration for the \<canvas\> element itself.
data CanvasConfig (c :: ContextType) t = CanvasConfig
  { _canvasConfig_El   :: RD.El t -- ^ The \<canvas\> element that will be used to extract the context object.
  , _canvasConfig_Args :: [Text]  -- ^ Any additional arguments to be used when calling the context function.
  }
makeLenses ''CanvasConfig

-- | Contains the context, a key press event function, as well as the raw
-- \<canvas\> element.
data CanvasInfo (c :: ContextType) t = CanvasInfo
  { _canvasInfo_El       :: RD.El t
  , _canvasInfo_context  :: RenderContext c
  , _canvasInfo_keyEvent :: RD.Key -> R.Event t ()
  }
makeLenses ''CanvasInfo

-- | Lawless typeclass to allow for overloading of the render function when
-- drawing instructions from the Free monad.
class IsRenderingContext c ~ IsRenderingContext (RenderContext a) => HasRenderFn a c | a -> c, c -> a where
  renderFunction :: c -> RenderFree a b -> JSM b

instance HasRenderFn 'TwoD CanvasRenderingContext2D where
  renderFunction = flip TwoD.drawToCanvas

instance HasRenderFn 'Webgl WebGLRenderingContext where
  renderFunction = flip GL.drawToCanvas
