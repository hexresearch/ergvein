{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Clay.Selector
import Clay.Display
import Clay.Stylesheet (prefixed)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Ergvein.Wallet.Embed
import Ergvein.Wallet.Embed.TH
import Ergvein.Wallet.Platform
import Language.Javascript.JSaddle hiding ((#))
import Prelude hiding ((**), rem)

import qualified Clay.Media as M
import qualified Clay.Flexbox as F

data Resources = Resources {
  robotoBlackUrl    :: !Text
, robotoBoldUrl     :: !Text
, robotoMediumUrl   :: !Text
, robotoRegularUrl  :: !Text
, fabrands400eotUrl    :: !Text
, fabrands400svgUrl    :: !Text
, fabrands400ttfUrl    :: !Text
, fabrands400woffUrl   :: !Text
, fabrands400woff2Url  :: !Text
, faregular400eotUrl   :: !Text
, faregular400svgUrl   :: !Text
, faregular400ttfUrl   :: !Text
, faregular400woffUrl  :: !Text
, faregular400woff2Url :: !Text
, fasolid900eotUrl     :: !Text
, fasolid900svgUrl     :: !Text
, fasolid900ttfUrl     :: !Text
, fasolid900woffUrl    :: !Text
, fasolid900woff2Url   :: !Text
}

embedResources :: MonadJSM m => m Resources
embedResources = Resources
  <$> createObjectURL robotBlack
  <*> createObjectURL robotoBold
  <*> createObjectURL robotoMedium
  <*> createObjectURL robotoRegular
  <*> createObjectURL fabrands400eot
  <*> createObjectURL fabrands400svg
  <*> createObjectURL fabrands400ttf
  <*> createObjectURL fabrands400woff
  <*> createObjectURL fabrands400woff2
  <*> createObjectURL faregular400eot
  <*> createObjectURL faregular400svg
  <*> createObjectURL faregular400ttf
  <*> createObjectURL faregular400woff
  <*> createObjectURL faregular400woff2
  <*> createObjectURL fasolid900eot
  <*> createObjectURL fasolid900svg
  <*> createObjectURL fasolid900ttf
  <*> createObjectURL fasolid900woff
  <*> createObjectURL fasolid900woff2

compileFrontendCss :: MonadJSM m => m ByteString
compileFrontendCss = do
  r <- embedResources
  pure $ frontendCssBS r

frontendCssBS :: Resources -> ByteString
frontendCssBS r = let
  selfcss = toStrict . encodeUtf8 . renderWith compact [] $ frontendCss r
  in milligramCss <> tooltipCss <> fontawesomeCss <> selfcss

frontendCss :: Resources -> Css
frontendCss r = do
  fontFamilies r
  faFontFamilies r
  html ? do
    textAlign center
    let px' = px 0 in padding px' px' px' px'
    let px' = px 0 in margin px' px' px' px'
    height $ pct 100
  body ? do
    color textColor
    backgroundColor majorBackground
    marginTop $ px 0
    marginLeft $ px 0
    marginRight $ px 0
    fontFamily ["Roboto"] []
    overflowY auto
  wrapperCss
  menuCss
  navbarCss
  buttonCss
  inputCss
  mnemonicWidgetCss
  validateCss
  passwordCss
  initialPageCss
  balancesPageCss
  sendPageCss
  networkPageCss
  aboutPageCss
  loadingWidgetCss
  alertsCss
  selectCss
  buttonsToggleCss
  graphPinCodeCanvasCss

textColor :: Color
textColor = rgb 0 0 0

hoverColor :: Color
hoverColor = rgb 112 112 112

majorBackground :: Color
majorBackground = rgb 255 255 255

minorBackground :: Color
minorBackground = rgb 59 78 122

wrapperCss :: Css
wrapperCss = do
  ".container" ? do
    position relative
    height $ pct 80
  ".vertical-center" ? do
    position absolute
    top $ pct 50
    left $ pct 50
    translatePctXY (pct (-50)) (pct (-50))
    width $ pct 100
    paddingLeft $ rem 2
    paddingRight $ rem 2

translatePctX :: Size Percentage -> Css
translatePctX x = prefixed (browsers <> "transform") $ "translateX(" <> value x <> ")"

translatePctY :: Size Percentage -> Css
translatePctY y = prefixed (browsers <> "transform") $ "translateY(" <> value y <> ")"

translatePctXY :: Size Percentage -> Size Percentage -> Css
translatePctXY x y = prefixed (browsers <> "transform") $ "translate(" <> value x <> ", " <> value y <> ")"

menuCss :: Css
menuCss = do
  ".menu-header" ? do
    width $ pct 100
    backgroundColor black
    color white
    fontSize $ pt 14
    paddingBottom $ px 10
    display displayTable
  ".menu-wallet-name" ? do
    display tableCell
    textAlign center
    width $ pct 100
    paddingTop $ px 5
  ".menu-wallet-menu" ? do
    display tableCell
    verticalAlign vAlignBottom
  ".menu-button" ? do
    width $ px 42
    marginRight $ px 10
  ".menu-dropdown-wrapper" ? do
    display inlineBlock
    position relative
  ".menu-dropdown" ? do
    display displayNone
    backgroundColor black
    minWidth $ px 160
    position absolute
    boxShadow [bsColor (rgba 0 0 0 0.2) $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)]
    zIndex 1
    translatePctX $ pct (-75)
    when isAndroid $ do
      marginTop $ px 10
      marginLeft $ px 5
  ".menu-dropdown .button.button-clear" ? do
    color white
    fontSize $ pt 14
    display block
    border solid (rem 0.1) white
    width $ pct 100
    borderRadius (px 0) (px 0) (px 0) (px 0)
    marginBottom $ px 0
  ".menu-dropdown-wrapper:hover .menu-dropdown" ? do
    display block

navbarCss :: Css
navbarCss = do
  ".navbar" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1, fr 1]
  ".navbar-item" ? do
    padding (rem 1) (rem 1) (rem 1) (rem 1)
    cursor pointer
  ".navbar-item:hover" ? do
    color hoverColor
  ".navbar-item.active" ? do
    borderBottom solid (px 4) textColor
  ".navbar-item.active:hover" ? do
    borderColor hoverColor

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
  let simpleBorder = border solid (rem 0.1) black
  let passInput = input # ("type" @= "password")
  (passInput # hover) <> (passInput # focus) ? simpleBorder
  let textInput = input # ("type" @= "text")
  (textInput # hover) <> (textInput # focus) ? simpleBorder

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

faFontFamilies :: Resources -> Css
faFontFamilies Resources{..} = do
  makeFontFace "Font Awesome 5 Brands" 400 [
      fabrands400eotUrl
    , fabrands400svgUrl
    , fabrands400ttfUrl
    , fabrands400woffUrl
    , fabrands400woff2Url
    ]
  makeFontFace "Font Awesome 5 Free" 400 [
      faregular400eotUrl
    , faregular400svgUrl
    , faregular400ttfUrl
    , faregular400woffUrl
    , faregular400woff2Url
    ]
  makeFontFace "Font Awesome 5 Free" 900 [
      fasolid900eotUrl
    , fasolid900svgUrl
    , fasolid900ttfUrl
    , fasolid900woffUrl
    , fasolid900woff2Url
    ]
  where
    makeFontFace name w urls = fontFace $ do
      fontFamily [name] []
      fontStyle normal
      fontFaceSrc [FontFaceSrcUrl url (Just format)
        | url    <- urls,
          format <- [EmbeddedOpenType, SVG, TrueType, WOFF, WOFF2]]
      fontWeight $ weight w

mnemonicWidgetCss :: Css
mnemonicWidgetCss = do
  ".mnemonic-word-dx" ? do
    fontFamily ["Roboto-Medium"] []
    fontSize $ pt 18
    textAlign $ alignSide sideLeft
  ".mnemonic-word-mb" ? do
    fontFamily ["Roboto-Medium"] []
    fontSize $ pt 18
    textAlign center
  ".mnemonic-word-ix" ? do
    fontSize $ em 0.6
    marginRight $ em 0.25
  ".mnemonic-warn" ? do
    marginTop $ px 30
  ".guess-buttons" ? do
    margin (px 0) auto (px 0) auto
  ".guess-button" ? do
    width $ pct 100
  ".restore-word" ? do
    minWidth $ px 120
  let mkGuess cl c = do
        cl ? backgroundColor c
        cl `with` hover ? backgroundColor c
        cl `with` focus ? backgroundColor c
  mkGuess ".guess-true" green
  mkGuess ".guess-false" red
  ".grid1" ? do
    display grid
    width maxContent
  ".grid3" ? do
    display grid
    width maxContent
  query M.screen [M.minWidth (rem 40)] $ ".grid3" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1, fr 1]
    gridGap $ rem 1
    width maxContent

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
  ".ask-password-modal" ? do
    position absolute
    top $ px 0
    left $ px 0
    width $ vw 100
    height $ vh 100
    zIndex 1
    backgroundColor white
    display flex
    flexDirection column
    justifyContent center
    paddingLeft $ pct 25
    paddingRight $ pct 25

initialPageCss :: Css
initialPageCss = do
  ".initial-options" ? do
    marginLeft auto
    marginRight auto
    marginBottom $ rem 1
  ".text-pin-code-error" ? do
    color $ rgb 190 0 0

balancesPageCss :: Css
balancesPageCss = do
  ".balances-wrapper" ? do
    maxWidth $ px 500
    margin (px 0) auto (px 0) auto
    textAlign $ alignSide sideLeft
  ".sync-progress" ? do
    fontSize $ pt 14
  ".currency-content" ? do
    display displayTable
    width $ pct 100
  ".currency-row" ? do
    display tableRow
    fontSize $ pt (if isAndroid then 18 else 24)
    cursor pointer
  ".currency-row:hover" ? do
    color hoverColor
  ".currency-name" ? do
    display tableCell
    paddingRight $ rem 1
  ".currency-balance" ? do
    display tableCell
    textAlign $ alignSide sideRight
  ".currency-value" ? do
    paddingRight $ rem 0.5
  ".currency-unit" ? do
    paddingRight $ rem 0.5

sendPageCss :: Css
sendPageCss = do
  ".send-wrapper" ? do
    maxWidth $ px 500
    margin (px 0) auto (px 0) auto
  ".send-buttons-wrapper" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
    gridGap $ rem 1
  ".send-submit" ? do
    width $ pct 100
  ".button-icon-wrapper" ? do
    marginLeft $ em 0.5

aboutPageCss :: Css
aboutPageCss = do
  ".about-wrapper" ? do
    textAlign center
  ".about-hr-sep" ? do
    border solid (px 3) black
  ".about-line" ? do
    width $ pct 100
    maxWidth $ px 500
    display inlineBlock
    textAlign center
  ".about-content" ? do
    display displayTable
    marginTop $ px 10
    fontSize $ pt (if isAndroid then 12 else 18)
  ".about-content-row" ? do
    display tableRow
  ".about-content-cell-label" ? do
    display tableCell
    let px' = px 5 in padding px' (px 20) px' px'
    textAlign $ alignSide sideLeft
    verticalAlign vAlignBottom
  ".about-content-cell-value" ? do
    display tableCell
    let px' = px 5 in padding px' px' px' px'
    textAlign $ alignSide sideLeft
    verticalAlign vAlignBottom
    width $ pct 1
  ".about-distrib" ? do
    paddingTop $ px 45
    fontSize $ pt (if isAndroid then 12 else 18)

networkPageCss :: Css
networkPageCss = do
  ".network-wrapper" ? do
    textAlign center
  ".network-title" ? do
    width $ pct 100
    maxWidth $ px 500
    display inlineBlock
  ".network-title-table" ? do
    display displayTable
  ".network-title-row" ? do
    display tableRow
  ".network-title-name" ? do
    display tableCell
    paddingTop $ px 15
    paddingRight $ px 3
    textAlign $ alignSide sideLeft
    width $ pct 65
  ".network-title-cur" ? do
    display tableCell
    paddingTop $ px 15
    paddingRight $ px 3
    width $ pct 35
    textAlign $ alignSide sideRight
  ".network-hr-sep" ? do
    marginTop $ px 5
    border solid (px 3) black
  ".network-hr-sep-lb" ? do
    border solid (px 1) black
  ".network-line" ? do
    width $ pct 100
    maxWidth $ px 500
    display inlineBlock
    textAlign center
  ".network-name" ? do
    display inlineBlock
    float floatLeft
    fontWeight bold
  ".network-name-edit" ? do
    display inlineBlock
    float floatRight
    fontWeight bold
    color "#3F7FBF"
  ".network-value" ? do
    display inlineBlock
    float floatLeft
    fontWeight bold
  ".network-descr" ? do
    display inlineBlock
    float floatLeft
    fontStyle italic
    fontSizeCustom smaller
  ".network-sel-cur-item" ? do
    textAlign center
    cursor pointer
    fontSize $ pt (if isAndroid then 12 else 18)

loadingWidgetCss :: Css
loadingWidgetCss = do
  ".loading-page" ? do
    backgroundColor $ rgba 0 0 0 0.75
    position absolute
    height $ pct 160
    width $ pct 100
    top $ px 0
    left $ px 0
    zIndex 1
    textAlign center
  ".loading-box" ? do
    display inlineBlock
    marginTop $ vh 50
    color white
  ".loading__bar" ? do
    border solid (px 2) "#9b4dca"
    width $ px 120
    height $ px 10
    boxSizing borderBox
  ".loading__status" ? do
    backgroundColor "#ab5dda"
    height $ pct 100

alertsCss :: Css
alertsCss = do
  ".alert-overlay" ? do
    pointerEvents none
    backgroundColor transparent
    position absolute
    bottom $ px 0
    right $ px 0
    width $ pct 100
  ".alert" ? fontWeight (weight 600)
  ".alert-handler" ? do
    zIndex 1
    marginLeft auto
    marginRight auto
    textAlign center
    let em1 = em 1 in padding em1 em1 em1 em1
    let px10 = px 10 in borderRadius px10 px10 px10 px10
    marginTop $ px 5
    marginBottom $ px 5
    marginLeft $ px 10
    marginRight $ px 10
    wordBreak breakAll
  ".badge" ? do
    let em' = em 0.75 in padding em' em' em' em'
    marginRight $ em 1
  ".alert-success" ? do
    color "#ffffff"
    backgroundColor "#88d68c"
  ".alert-info" ? do
    color "#ffffff"
    backgroundColor "#3fa7d8"
  ".alert-danger" ? do
    color "#ffffff"
    backgroundColor "#d64d35"
  ".alert-secondary" ? do
    color "#000"
    backgroundColor "#a9a7a7"

patternKeyCss :: Css
patternKeyCss = do
  ".myTestDiv" ? do
    pointerEvents none
    backgroundColor "red"
    display block
  ".myDebugLog" ? do
    display block

selectCss :: Css
selectCss = do
  ".select-lang" ? do
    fontSize $ pt 18
    height   $ em 1.8
  "option" ? do
    fontSize $ pt 18
    height   $ em 1.8

buttonsToggleCss :: Css
buttonsToggleCss = do
  ".button-on" ? do
    fontSize $ pt 18
    width $ px 200
    backgroundColor "#000000"
    color "#ffffff"
  ".button-off" ? do
    fontSize $ pt 18
    width $ px 200
    backgroundColor "#ffffff"
    color "#000000"
  ".button-not-working" ? do
    visibility hidden
    pointerEvents none

graphPinCodeCanvasCss :: Css
graphPinCodeCanvasCss = do
  ".graph-pin-code-canvas" ? do
    position relative
    backgroundColor $ rgb 240 240 240
    border solid (px 1) black
    borderRadius (px 5) (px 5) (px 5) (px 5)
    let px' = px 0 in padding px' px' px' px'
    marginLeft auto
    marginRight auto
    userSelect none
    cursor pointer
    zIndex 3
  ".graph-pin-code-canvas-error" ? do
    position relative
    backgroundColor $ rgb 255 230 230
    border solid (px 1) $ rgb 190 0 0
    borderRadius (px 5) (px 5) (px 5) (px 5)
    let px' = px 0 in padding px' px' px' px'
    marginLeft auto
    marginRight auto
    userSelect none
    cursor pointer
    zIndex 3
  ".graph-pin-code-point" ? do
    position absolute
    backgroundColor $ rgb 140 140 140
    let px' = px 0 in padding px' px' px' px'
    let px' = px 0 in margin px' px' px' px'
    let pct' = pct 50 in borderRadius pct' pct' pct' pct'
    userSelect none
    cursor pointer
    zIndex 5
  ".graph-pin-code-glass" ? do
    position absolute
    backgroundColor none
    let px' = px 0 in padding px' px' px' px'
    let px' = px 0 in margin px' px' px' px'
    let pct' = pct 50 in borderRadius pct' pct' pct' pct'
    userSelect none
    zIndex 10
  ".graph-pin-code-point-check" ? do
    position absolute
    backgroundColor $ rgb 90 90 90
    let px' = px 0 in padding px' px' px' px'
    let px' = px 0 in margin px' px' px' px'
    let pct' = pct 50 in borderRadius pct' pct' pct' pct'
    userSelect none
    cursor pointer
    zIndex 10
  ".graph-pin-code-line-check" ? do
    position absolute
    backgroundColor $ none
    let px' = px 0 in padding px' px' px' px'
    let px' = px 0 in margin px' px' px' px'
    userSelect none
    cursor pointer
    borderTop solid (px 2) $ rgb 90 90 90
    height $ px 2
    zIndex 10
