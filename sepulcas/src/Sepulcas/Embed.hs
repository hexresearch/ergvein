module Sepulcas.Embed(
    createObjectURL
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)
import Foreign.JavaScript.Utils (bsToArrayBuffer)
import Language.Javascript.JSaddle
import Data.Maybe

import qualified GHCJS.DOM.Blob as Blob
import qualified GHCJS.DOM.Types as JS

createObjectURL_ :: MonadJSM m => Blob.Blob -> m JS.JSVal
createObjectURL_ b = liftJSM $ do
  void $ eval ("sepulcas_createObjectURL = function(a) { return window.URL.createObjectURL(a); }" :: Text)
  liftJSM $ jsg1 ("sepulcas_createObjectURL" :: Text) (toJSVal b)

createObjectURL :: MonadJSM m => ByteString -> m Text
createObjectURL bs = do
  let opt :: Maybe JS.BlobPropertyBag
      opt = Nothing
  ba <- bsToArrayBuffer bs
  b <- Blob.newBlob [ba] opt
  jurl <- createObjectURL_ b
  url <- liftJSM $ JS.fromJSVal jurl
  return $ fromMaybe "" url
