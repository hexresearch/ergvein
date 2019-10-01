-- | Module that contains embedded CSS files from statics to reduce compilation
-- time as TemplateHaskell is quite slow in GHCJS.
module Ergvein.Wallet.Style.TH(
    milligramCss
  , tooltipCss
  ) where

import Data.FileEmbed
import Data.ByteString (ByteString)

milligramCss :: ByteString
milligramCss = $(embedFile "statics/css/milligram.min.css")

tooltipCss :: ByteString
tooltipCss = $(embedFile "statics/css/tooltip.css")
