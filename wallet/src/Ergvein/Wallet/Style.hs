module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Ergvein.Wallet.Style.TH
import Language.Javascript.JSaddle
import Prelude hiding ((**), rem)

compileFrontendCss :: MonadJSM m => m ByteString
compileFrontendCss = pure frontendCssBS

frontendCssBS :: ByteString
frontendCssBS = let
  selfcss = toStrict . encodeUtf8 . renderWith compact [] $ frontendCss
  in milligramCss <> tooltipCss <> selfcss

frontendCss :: Css
frontendCss = do
  html ? textAlign center
  body ? do
    color textColor
    backgroundColor majorBackground

textColor :: Color
textColor = rgb 0 0 0

majorBackground :: Color
majorBackground = rgb 255 255 255

minorBackground :: Color
minorBackground = rgb 59 78 122
