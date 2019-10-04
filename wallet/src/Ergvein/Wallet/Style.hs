module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Language.Javascript.JSaddle
import Prelude hiding ((**), rem)

data Resources = Resources {
  robotoBlackUrl   :: !Text
, robotoBoldUrl    :: !Text
, robotoMediumUrl  :: !Text
, robotoRegularUrl :: !Text
}

embedResources :: MonadJSM m => m Resources
embedResources = Resources
  <$> createObjectURL robotBlack
  <*> createObjectURL robotoBold
  <*> createObjectURL robotoMedium
  <*> createObjectURL robotoRegular

compileFrontendCss :: MonadJSM m => m ByteString
compileFrontendCss = do
  r <- embedResources
  pure $ frontendCssBS r

frontendCssBS :: Resources -> ByteString
frontendCssBS r = let
  selfcss = toStrict . encodeUtf8 . renderWith compact [] $ frontendCss r
  in milligramCss <> tooltipCss <> selfcss

frontendCss :: Resources -> Css
frontendCss r = do
  fontFamilies r
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

fontFamilies :: Resources -> Css
fontFamilies Resources{..} = pure ()
