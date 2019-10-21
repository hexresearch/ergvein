module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Clay.Selector
import Clay.Stylesheet (prefixed)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Language.Javascript.JSaddle hiding ((#))
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
  wrapperCss
  buttonCss
  inputCss
  mnemonicWidgetCss
  validateCss
  passwordCss
  initialPageCss

textColor :: Color
textColor = rgb 0 0 0

majorBackground :: Color
majorBackground = rgb 255 255 255

minorBackground :: Color
minorBackground = rgb 59 78 122

wrapperCss :: Css
wrapperCss = do
  ".container" ? do
    position relative
    height $ pct 100
  ".vertical-center" ? do
    margin (px 0) (px 0) (px 0) (px 0)
    position absolute
    top $ pct 50
    translatePctY $ pct (-50)
    width $ pct 100
    where
      translatePctY y = prefixed (browsers <> "transform") $ "translateY(" <> value y <> ")"

buttonCss :: Css
buttonCss = do
  let submit = input # ("type" @= "submit")
      submitOutline = submit # byClass "button-outline"
      submitClear = submit # byClass "button-clear"
  ".button.button-outline" <> submitOutline ? color black
  ".button.button-clear" <> submitClear ? color black
  ".button" <> submit ? border solid (rem 0.1) black
  ".back-button" ? do
    textAlign $ alignSide sideLeft
  ".back-button" ** button ? do
    fontSize $ pt 12

inputCss :: Css
inputCss = do
  let passInput = input # ("type" @= "password")
  (passInput # hover) <> (passInput # focus) ? border solid (rem 0.1) black

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

validateCss :: Css
validateCss = do
  ".validate-error" ? do
    fontSize $ pt 14

passwordCss :: Css
passwordCss = do
  ".password-field" ? do
    position relative
    display inlineBlock
    width $ pct 100
  ".eyed-field" ? do
    textAlign $ alignSide sideLeft
    width $ pct 100
  ".small-eye" ? do
    width $ px 26
    height $ px 14
    position absolute
    top $ px 0
    right $ px 0
    marginTop $ px 10
    marginRight $ px 13

initialPageCss :: Css
initialPageCss = do
  ".initial-options" ** button ? do
    width $ pct 80
    marginLeft auto
    marginRight auto
    marginBottom $ px 10
