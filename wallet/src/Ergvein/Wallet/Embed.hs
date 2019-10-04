module Ergvein.Wallet.Embed(
    createObjectURL
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Foreign.JavaScript.Utils (bsToArrayBuffer)
import Language.Javascript.JSaddle
import Data.Maybe

import qualified GHCJS.DOM.Blob as Blob
import qualified GHCJS.DOM.Types as JS
import qualified Data.Text as T

createObjectURL_ :: MonadJSM m => Blob.Blob -> m JS.JSVal
createObjectURL_ b = liftJSM $ jsg1 ("window.URL.createObjectURL" :: Text) (toJSVal b)

createObjectURL :: MonadJSM m => ByteString -> m Text
createObjectURL bs = do
  let opt :: Maybe JS.BlobPropertyBag
      opt = Nothing
  ba <- bsToArrayBuffer bs
  b <- Blob.newBlob [ba] opt
  jurl <- createObjectURL_ b
  url <- liftJSM $ JS.fromJSVal jurl
  return $ fromMaybe "" url
