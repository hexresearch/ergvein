module Ergvein.Wallet.Style(
    frontendCss
  , frontendCssBS
  ) where

import Clay
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Ergvein.Wallet.Style.TH
import Prelude hiding ((**), rem)

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
textColor = rgb 179 184 229

majorBackground :: Color
majorBackground = rgb 24 41 82

minorBackground :: Color
minorBackground = rgb 59 78 122
