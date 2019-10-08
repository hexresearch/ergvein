module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Clay.Selector
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
    fontFamily ["Roboto"] []
  buttonCss
  mnemonicWidgetCss

textColor :: Color
textColor = rgb 0 0 0

majorBackground :: Color
majorBackground = rgb 255 255 255

minorBackground :: Color
minorBackground = rgb 59 78 122

buttonCss :: Css
buttonCss = do
  ".button.button-outline" ? color black
  ".button" ? border solid (rem 0.1) black

fontFamilies :: Resources -> Css
fontFamilies Resources{..} = do
  makeFontFace "Roboto" robotoRegularUrl
  makeFontFace "Roboto-Bold" robotoBoldUrl
  makeFontFace "Roboto-Black" robotoBlackUrl
  makeFontFace "Roboto-Medium" robotoMediumUrl
  where
    makeFontFace name url = fontFace $ do
      fontFamily [name] []
      fontFaceSrc [FontFaceSrcUrl url (Just TrueType)]
      fontWeight $ weight 400

mnemonicWidgetCss :: Css
mnemonicWidgetCss = do
  ".mnemonic-word" ? do
    fontFamily ["Roboto-Medium"] []
    fontSize $ pt 18
  ".mnemonic-warn" ? do
    marginTop $ px 30
  ".guess-buttons" ? textAlign center
  ".guess-button" ? do
    marginRight $ px 30
    display inlineBlock
  let mkGuess cl c = do
        cl ? backgroundColor c
        cl `with` hover ? backgroundColor c
        cl `with` focus ? backgroundColor c
  mkGuess ".guess-true" green
  mkGuess ".guess-false" red
