{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Subset of the <https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API WebGL API> built as a 'Control.Monad.Free.Church.Free' monad.
--
-- These are experimental and do not cover even half of the API, it is likely
-- they will be wholly deprecated in the near future.
module Reflex.Dom.Canvas.WebGL where

import           GHCJS.DOM.Types                     (ArrayBuffer, GLboolean,
                                                      GLclampf, GLenum, GLfloat,
                                                      GLint, GLintptr, GLsizei,
                                                      GLuint, JSString, JSVal,
                                                      MonadJSM, ToJSString,
                                                      WebGLBuffer, WebGLProgram,
                                                      WebGLShader,
                                                      WebGLUniformLocation,
                                                      fromJSVal, liftJSM,
                                                      toJSString)

import           Data.Either                         (Either (..))
import           Data.Maybe                          (fromMaybe)
import           Data.Text                           (Text)

import qualified GHCJS.DOM.WebGLRenderingContextBase as WebGL

import           Control.Monad.Free.Church

-- my my, aren't shaders and programs similar...PREPARE THE WIFFLE BAT OF TYPE SAFETY.
data WebGLF a
  -- Shader Mangling
  = CreateShader GLenum (WebGLShader -> a)
  | ShaderSource WebGLShader JSString a
  | CompileShader WebGLShader a
  | GetShaderParameter WebGLShader GLenum (JSVal -> a)
  | GetShaderInfoLog WebGLShader (Maybe JSString -> a)
  | DeleteShader WebGLShader a

  -- Program Mangling
  | CreateProgram (WebGLProgram -> a)
  | AttachShader WebGLProgram WebGLShader a
  | LinkProgram WebGLProgram a
  | GetProgramParameter WebGLProgram GLenum (JSVal -> a)
  | GetProgramInfoLog WebGLProgram (Maybe JSString -> a)
  | DeleteProgram WebGLProgram a
  | UseProgram WebGLProgram a

  -- Attribbles
  | GetAttribLocation WebGLProgram JSString (GLint -> a)
  | EnableVertexAttribArray GLuint a
  | VertexAttribPointer GLuint GLint GLenum GLboolean GLsizei GLintptr a

  -- Uniforms
  | GetUniformLocation WebGLProgram JSString (WebGLUniformLocation -> a)
  | Uniform2f WebGLUniformLocation GLfloat GLfloat a
  | Uniform4f WebGLUniformLocation GLfloat GLfloat GLfloat GLfloat a

  -- Buffers
  | CreateBuffer (WebGLBuffer -> a)
  | BindBuffer GLenum WebGLBuffer a
  | BufferData GLenum ArrayBuffer GLenum a

  -- Viewport
  | Viewport GLint GLint GLsizei GLsizei a

  -- Colour
  | ClearColour GLclampf GLclampf GLclampf GLclampf a
  | Clear GLenum a

  -- Get Busy
  | DrawArrays GLenum GLint GLsizei a

  -- Reading JS values
  | ReadJSBool Bool JSVal (Bool -> a)

  --
  | Noop a
  deriving Functor

type WebGLM = F WebGLF

liftF' :: MonadFree WebGLF m => ((a -> a) -> WebGLF a) -> m a
liftF' = liftF . ($ id)

liftF_ :: MonadFree WebGLF m => (() -> WebGLF ()) -> m ()
liftF_ = liftF . ($ ())

noopF :: MonadFree WebGLF m => m ()
noopF = liftF_ Noop

-- Reading JS Values
readJSBoolF :: MonadFree WebGLF m => Bool -> JSVal -> m Bool
readJSBoolF d = liftF' . ReadJSBool d

-- Get Busy

drawArraysF :: MonadFree WebGLF m => GLenum -> GLint -> GLsizei -> m ()
drawArraysF gle i1 = liftF_ . DrawArrays gle i1

-- Colour

clearColourF :: MonadFree WebGLF m => GLclampf -> GLclampf -> GLclampf -> GLclampf -> m ()
clearColourF c1 c2 c3 = liftF_ . ClearColour c1 c2 c3

clearF :: MonadFree WebGLF m => GLenum -> m ()
clearF = liftF_ . Clear

-- Viewport

viewportF :: MonadFree WebGLF m => GLint -> GLint -> GLsizei -> GLsizei -> m ()
viewportF i1 i2 s1 = liftF_ . Viewport i1 i2 s1

-- Buffers

createBufferF :: MonadFree WebGLF m => m WebGLBuffer
createBufferF = liftF' CreateBuffer

bindBufferF :: MonadFree WebGLF m => GLenum -> WebGLBuffer -> m ()
bindBufferF gle = liftF_ . BindBuffer gle

bufferDataF
  :: ( MonadFree WebGLF m
     -- , IsBufferDataSource buff
     )
  => GLenum
  -> ArrayBuffer
  -> GLenum
  -> m ()
bufferDataF gle1 buff =
  liftF_ . BufferData gle1 buff

-- Uniforms

getUniformLocationF
  :: ( MonadFree WebGLF m
     , ToJSString location
     )
  => WebGLProgram
  -> location
  -> m WebGLUniformLocation
getUniformLocationF p =
  liftF' . GetUniformLocation p . toJSString

uniform2fF
  :: MonadFree WebGLF m
  => WebGLUniformLocation
  -> GLfloat
  -> GLfloat
  -> m ()
uniform2fF location v1 =
  liftF_ . Uniform2f location v1

uniform4fF
  :: MonadFree WebGLF m
  => WebGLUniformLocation
  -> GLfloat
  -> GLfloat
  -> GLfloat
  -> GLfloat
  -> m ()
uniform4fF location v1 v2 v3 =
  liftF_ . Uniform4f location v1 v2 v3

-- Attribbles
getAttribLocationF
  :: ( MonadFree WebGLF m
     , ToJSString location
     )
  => WebGLProgram
  -> location
  -> m GLint
getAttribLocationF p =
  liftF' . GetAttribLocation p . toJSString

enableVertexAttribArrayF :: MonadFree WebGLF m => GLuint -> m ()
enableVertexAttribArrayF = liftF_ . EnableVertexAttribArray

vertexAttribPointerF
  :: MonadFree WebGLF m
  => GLuint
  -> GLint
  -> GLenum
  -> GLboolean
  -> GLsizei
  -> GLintptr
  -> m ()
vertexAttribPointerF index size type' normalized stride =
  liftF_ . VertexAttribPointer index size type' normalized stride

-- Programs
createProgramF :: MonadFree WebGLF m => m WebGLProgram
createProgramF = liftF' CreateProgram

attachShaderF :: MonadFree WebGLF m => WebGLProgram -> WebGLShader -> m ()
attachShaderF p = liftF_ . AttachShader p

linkProgramF :: MonadFree WebGLF m => WebGLProgram -> m ()
linkProgramF = liftF_ . LinkProgram

getProgramParameterF :: MonadFree WebGLF m => WebGLProgram -> GLenum -> m JSVal
getProgramParameterF p = liftF' . GetProgramParameter p

getProgramInfoLogF :: MonadFree WebGLF m => WebGLProgram -> m (Maybe JSString)
getProgramInfoLogF = liftF' . GetProgramInfoLog

deleteProgramF :: MonadFree WebGLF m => WebGLProgram -> m ()
deleteProgramF = liftF_ . DeleteProgram

useProgramF :: MonadFree WebGLF m => WebGLProgram -> m ()
useProgramF = liftF_ . UseProgram

-- Shaders
createShaderF :: MonadFree WebGLF m => GLenum -> m WebGLShader
createShaderF = liftF' . CreateShader

shaderSourceF :: ( MonadFree WebGLF m, ToJSString string ) => WebGLShader -> string -> m ()
shaderSourceF shdr = liftF_ . ShaderSource shdr . toJSString

compileShaderF :: MonadFree WebGLF m => WebGLShader -> m ()
compileShaderF = liftF_ . CompileShader

getShaderParameterF :: MonadFree WebGLF m => WebGLShader -> GLenum -> m JSVal
getShaderParameterF s = liftF' . GetShaderParameter s

getShaderInfoLogF :: MonadFree WebGLF m => WebGLShader -> m (Maybe JSString)
getShaderInfoLogF = liftF' . GetShaderInfoLog

deleteShaderF :: MonadFree WebGLF m => WebGLShader -> m ()
deleteShaderF = liftF_ . DeleteShader

buildShader
  :: MonadFree WebGLF m
  => Text
  -> GLenum
  -> m (Either JSString WebGLShader)
buildShader src sType = do
  let defErr = "Unknown Shader Compilation Error Occurred!"
  s <- createShaderF sType
  shaderSourceF s src
  compileShaderF s
  compileOkay <- readJSBoolF False =<<
    getShaderParameterF s WebGL.COMPILE_STATUS
  if compileOkay
    then pure (Right s)
    else getShaderInfoLogF s >>= pure . Left . fromMaybe defErr

buildProgram
  :: MonadFree WebGLF m
  => WebGLShader
  -> WebGLShader
  -> m ( Either JSString WebGLProgram )
buildProgram vert frag = do
  let defErr = "Unknown Program Linking Error Occurred"
  p <- createProgramF
  attachShaderF p vert
  attachShaderF p frag
  linkProgramF p
  linkOkay <- readJSBoolF False =<<
    getProgramParameterF p WebGL.LINK_STATUS
  if linkOkay
    then pure (Right p)
    else getProgramInfoLogF p >>= pure . Left . fromMaybe defErr

drawToCanvas
  :: ( WebGL.IsWebGLRenderingContextBase cx, MonadJSM m )
  => F WebGLF a
  -> cx
  -> m a
drawToCanvas instructions cxt =
  foldF (applyInstruction cxt) instructions

applyInstruction
  :: ( WebGL.IsWebGLRenderingContextBase cx
     , MonadJSM m
     )
  => cx
  -> WebGLF a
  -> m a
applyInstruction cxt instruction = liftJSM $
  case instruction of
    -- Shader Mangling
    CreateShader glE cont              -> cont <$> WebGL.createShader cxt glE
    ShaderSource shader src cont       -> cont <$  WebGL.shaderSource cxt ( Just shader ) src
    CompileShader shader cont          -> cont <$  WebGL.compileShader cxt ( Just shader )
    GetShaderParameter shader gle cont -> cont <$> WebGL.getShaderParameter cxt ( Just shader ) gle
    GetShaderInfoLog shader cont       -> cont <$> WebGL.getShaderInfoLog cxt ( Just shader )
    DeleteShader shader cont           -> cont <$  WebGL.deleteShader cxt ( Just shader )

      -- Program Mangling
    CreateProgram cont                     -> cont <$> WebGL.createProgram cxt
    AttachShader glProgram shader cont     -> cont <$  WebGL.attachShader cxt ( Just glProgram ) ( Just shader )
    LinkProgram glProgram cont             -> cont <$  WebGL.linkProgram cxt ( Just glProgram )
    GetProgramParameter glProgram gle cont -> cont <$> WebGL.getProgramParameter cxt ( Just glProgram ) gle
    GetProgramInfoLog glProgram cont       -> cont <$> WebGL.getProgramInfoLog cxt ( Just glProgram )
    DeleteProgram glProgram cont           -> cont <$  WebGL.deleteProgram cxt ( Just glProgram )
    UseProgram glProgram cont              -> cont <$  WebGL.useProgram cxt ( Just glProgram )

      -- Attribbles
    GetAttribLocation glProgram loc cont -> cont <$> WebGL.getAttribLocation cxt ( Just glProgram ) loc
    EnableVertexAttribArray arrLoc cont  -> cont <$  WebGL.enableVertexAttribArray cxt arrLoc
    VertexAttribPointer arrLoc size dataType normalise stride offset cont ->
      cont <$ WebGL.vertexAttribPointer cxt arrLoc size dataType normalise stride offset

      -- Uniforms
    GetUniformLocation glProgram loc cont -> cont <$> WebGL.getUniformLocation cxt ( Just glProgram ) loc
    Uniform2f unifLoc a b cont            -> cont <$ WebGL.uniform2f cxt ( Just unifLoc ) a b
    Uniform4f unifLoc a b c d cont        -> cont <$ WebGL.uniform4f cxt ( Just unifLoc ) a b c d

      -- Buffers
    CreateBuffer cont                           -> cont <$> WebGL.createBuffer cxt
    BindBuffer buffType buff cont               -> cont <$ WebGL.bindBuffer cxt buffType ( Just buff )
    BufferData buffType buffData buffUsage cont -> cont <$ WebGL.bufferData cxt buffType ( Just buffData ) buffUsage

      -- Viewport
    Viewport x y w h cont -> cont <$ WebGL.viewport cxt x y w h

      -- Colour
    ClearColour r g b a cont -> cont <$ WebGL.clearColor cxt r g b a
    Clear bit cont           -> cont <$ WebGL.clear cxt bit

      -- Get Busy
    DrawArrays primType offset count cont -> cont <$ WebGL.drawArrays cxt primType offset count

      -- Reading JS values
    ReadJSBool defBool inp cont -> cont . fromMaybe defBool <$> fromJSVal inp

    Noop cont -> pure cont
