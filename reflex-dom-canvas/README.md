![CSIRO's Data61 Logo](https://raw.githubusercontent.com/qfpl/assets/master/data61-transparent-bg.png)

# Reflex HTML5 Canvas

Helper functions for creating and managing a [Canvas](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas) element and a [RenderingContext](https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext). Supports both 2d and WebGL.

The main functions for creating the canvas and retrieving the ``Dynamic`` containing the requested context are in ``Reflex.Dom.CanvasDyn``. Use either ``dContext2d`` or ``dContextWebgl`` to build your canvas. You will need to provide a configuration record:

```haskell
data CanvasConfig (c :: ContextType) t = CanvasConfig
  { -- This is the <canvas> element that you created with Reflex.Dom
    _canvasConfig_El   :: RD.El t
    -- This is a list of stringly arguments that are passed along to the JavaScript 'getContext' function.
  , _canvasConfig_Args :: [Text]
  }
```
The ``(c :: ContextType)`` type variable will be set according to the type of context you are requesting. You should not need to provide it explicitly and is there to prevent you from passing WebGL instructions to a 2D canvas, and vice versa:

```haskell
exampleFoo = do
  let
    canvasAttrs = Map.fromList
      [ ("height", "480")
      , ("width" , "640")
      , ("id"    , "fruity")
      ]

  -- Create the canvas element, using the backticked el function so Reflex.Dom provides us
  -- with the `El t`, which is the representation of the <canvas> element.
  ( canvasEl, _ ) <- RD.elAttr' "canvas" canvasAttrs RD.blank

  -- Retrieve our CanvasInfo, restricted to 'context2d' because of the types! Yay! :D
  d2D <- dContext2d ( Canvas.CanvasConfig canvasEl [] )
  ...
  ...
```

This ``CanvasInfo`` will be in a ``Dynamic`` with the following fields:

```haskell
data CanvasInfo (c :: ContextType) t = CanvasInfo
  { -- Canvas HTML element
    _canvasInfo_El       :: El t
    -- RenderingContext JS object for the context (2d/webgl) that you can requested.
  , _canvasInfo_context  :: RenderContext c
    -- Function, takes a Key, returns an Event for when that Key is pressed on the Canvas element
  , _canvasInfo_keyEvent :: Key -> Event t ()
  }
```

The functions:
```haskell
drawWithCx
  :: ( MonadWidget t m
     , HasRenderFn c ( RenderContext c )
     )
  => Dynamic t ( RenderContext c )
  -> Dynamic t ( RenderContext c -> Double -> JSM a )
  -> Event t ()
  -> m ( Event t a )
```
and
```haskell
drawCanvasFree
  :: ( MonadWidget t m
     , HasRenderFn c ( RenderContext c )
     )
  => Dynamic t ( RenderFree c a )
  -> Dynamic t ( RenderContext c )
  -> Event t ()
  -> m (Event t a)
```
are for applying instructions to the canvas using ``Dynamic t ( RenderContext c )``.

There are two modules ``Reflex.Dom.Canvas.WebGL`` and ``Reflex.Dom.Canvas.Context2D`` that contain a ``Free`` monad implementation of some canvas functions for their respective contexts. These are not complete implementations, but may be useful to someone wanting to experiment with a more FP friendly way of interacting with the Canvas API.
