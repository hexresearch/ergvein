module Sepulcas.Style(
    compileStyles
  ) where

import Clay
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Javascript.JSaddle hiding ((#))
import Sepulcas.Style.Embed
import Sepulcas.Style.Fonts

compileStyles :: MonadJSM m => Css -> m ByteString
compileStyles userStyles = frontendCssBS userStyles <$> embedFonts

frontendCssBS :: Css -> Fonts -> ByteString
frontendCssBS userStyles r = let
  selfcss = toStrict . encodeUtf8 . renderWith compact [] $ fontStyles r >> userStyles
  in milligramCss <> tooltipCss <> fontawesomeCss <> selfcss
