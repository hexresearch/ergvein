{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Clay.Selector
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
  robotoBlackUrl       :: !Text
, robotoBoldUrl        :: !Text
, robotoMediumUrl      :: !Text
, robotoRegularUrl     :: !Text
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
  htmlCss
  bodyCss
  aboutPageCss
  alertsCss
  balancesPageCss
  buttonCss
  buttonsToggleCss
  graphPinCodeCanvasCss
  headerCss
  historyPageCss
  txInfoPageCss
  bumpFeePageCss
  infoPageCss
  initialPageCss
  inputCss
  legoStyles
  linkCss
  loadingWidgetCss
  mnemonicWidgetCss
  navbarCss
  networkPageCss
  toggleSwitchCss
  passwordCss
  badgeCss
  receiveCss
  selectCss
  sendPageCss
  settingsCss
  mnemonicExportCss
  sharePageCss
  validateCss
  wrapperCss
  testnetDisclaimerCss

textColor :: Color
textColor = rgb 0 0 0

hoverColor :: Color
hoverColor = rgb 112 112 112

disabledColor :: Color
disabledColor = rgb 233 236 239

textSuccess :: Color
textSuccess = rgb 40 167 69

textWarning :: Color
textWarning = rgb 255 193 7

textDanger :: Color
textDanger = rgb 220 53 69

textInfo :: Color
textInfo = rgb 23 162 184

textInfo2 :: Color
textInfo2 = rgb 220 220 215

majorBackground :: Color
majorBackground = rgb 255 255 255

minorBackground :: Color
minorBackground = rgb 59 78 122

mobileBreakpoint :: Size LengthUnit
mobileBreakpoint = rem 40

tabletBreakpoint :: Size LengthUnit
tabletBreakpoint = rem 80

desktopBreakpoint :: Size LengthUnit
desktopBreakpoint = rem 120

htmlCss :: Css
htmlCss = do
  html ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    textAlign center
  "input, textarea, select" ? do
    fontFamily ["Roboto"] [sansSerif, monospace]
    fontSize $ pt 14

bodyCss :: Css
bodyCss = body ? do
  margin (px 0) (px 0) (px 0) (px 0)
  padding (px 0) (px 0) (px 0) (px 0)
  color textColor
  backgroundColor majorBackground
  fontFamily ["Roboto"] [sansSerif, monospace]

wrapperCss :: Css
wrapperCss = do
  ".wrapper" ? do
    minHeight $ vh 100
    display flex
    flexDirection column
  ".wrapper .container" ? do
    maxWidth tabletBreakpoint
    flexGrow 1
  ".centered-container" ? do
    display flex
    flexGrow 1
  ".centered-content" ? do
    margin auto auto auto auto

headerCss :: Css
headerCss = do
  ".header-wrapper" ? do
    position sticky
    top $ rem 0
    zIndex 1
  ".header" ? do
    display flex
    fontSize $ pt 14
  ".header-black" ? do
    backgroundColor black
    color white
  ".header-wallet-text" ? do
    width $ pct 100
    padding (rem 1) (rem 1) (rem 1) (rem 1)
  ".header-button" ? do
    fontSize $ pt 20
    padding (rem 1) (rem 1) (rem 1) (rem 1)
  ".header-button:hover" ? do
    cursor pointer
    color hoverColor
  ".header-button.hidden" ? do
    visibility hidden
  ".menu" ? do
    position absolute
    right $ rem 0
    backgroundColor black
    color white
    boxShadow [bsColor (rgba 0 0 0 0.2) $ shadowWithSpread (px 0) (px 8) (px 16) (px 0)]
  ".menu .button.button-clear" ? do
    color white
    fontSize $ pt 14
    display block
    border solid (rem 0.1) white
    width $ pct 100
    borderRadius (px 0) (px 0) (px 0) (px 0)
    marginBottom $ px 0
  ".menu.hidden" ? do
    display displayNone
  ".menu-android-wrapper" ? do
    position fixed
    top $ rem 0
    left $ rem 0
    width $ pct 100
    height $ pct 100
    zIndex 1
    overflowY auto
    overflowX hidden
    backgroundColor $ rgba 0 0 0 0.5
    transition "" (sec 0.3) easeOut (sec 0)
  ".menu-android-wrapper.hidden" ? do
    zIndex (-1)
    backgroundColor $ rgba 0 0 0 0
  ".menu-android" ? do
    position absolute
    top $ rem 0
    left $ pct 20
    width $ pct 80
    height $ pct 100
    backgroundColor black
    color white
    transition "" (sec 0.3) easeOut (sec 0)
    zIndex 2
    display flex
    flexDirection column
  ".menu-android.hidden" ? do
    left $ pct 100
    width $ pct 0
  ".menu-android-buttons-wrapper" ? do
    display flex
    flexDirection column
    alignItems flexStart
    padding (rem 0) (rem 1) (rem 0) (rem 4)
  ".menu-android-header" ? do
    display flex
  ".menu-android-close-button" ? do
    marginLeft auto
  ".menu-android-button" ? do
    marginBottom $ rem 2
    fontSize $ pt 16
    paddingBottom $ rem 0.3
    borderBottom solid (rem 0.1) hoverColor
    whiteSpace nowrap
  ".menu-android-button-icon" ? do
    marginRight $ rem 0.7
  ".menu-android-button:hover" ? do
    cursor pointer
    color hoverColor

navbarCss :: Css
navbarCss = do
  ".navbar-2-cols" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
    padding (rem 0) (rem 1) (rem 0) (rem 1)
  ".navbar-3-cols" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1, fr 1]
    padding (rem 0) (rem 1) (rem 0) (rem 1)
  ".navbar-5-cols" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1, fr 1, fr 1, fr 1]
    padding (rem 0) (rem 1) (rem 0) (rem 1)
  ".navbar-item" ? do
    padding (rem 1) (rem 1) (rem 1) (rem 1)
    cursor pointer
  ".navbar-item:hover" ? do
    color hoverColor
  ".navbar-item.active" ? do
    borderBottom solid (px 4) textColor
  ".navbar-item.active:hover" ? do
    borderColor hoverColor
  ".navbar-black" ? do
    backgroundColor black
    color white
  ".navbar-balance" ? do
    display flex
    justifyContent center
    fontSize $ pt 20
    marginBottom $ rem 1
  ".navbar-status" ? do
    display flex
    justifyContent center
    fontSize $ pt 12
    marginBottom $ rem 1
    color silver
  ".navbar-android-controls-wrapper" ? do
    display flex
    justifyContent center
    paddingBottom $ rem 2
    maxWidth $ rem 22
    width $ pct 100
    margin (px 0) auto (px 0) auto
  ".navbar-android-controls" ? do
    display flex
    justifyContent spaceBetween
    paddingBottom $ rem 2
    maxWidth $ rem 22
    width $ pct 100
  ".navbar-android-controls-button" ? do
    display flex
    flexDirection column
    justifyContent center
    alignItems center
    fontSize $ pt 14
    cursor pointer
  ".navbar-android-controls-button:hover" ? do
    opacity 0.7
  ".navbar-android-controls-button-icon" ? do
    display inlineFlex
    justifyContent center
    alignItems center
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    backgroundColor $ rgb 50 50 50
    width $ rem 6
    height $ rem 6
    fontSize $ pt 16

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
      disableBackground = backgroundColor disabledColor
  let passInput = input # ("type" @= "password")
  (passInput # hover) <> (passInput # focus) ? simpleBorder
  let textInput = input # ("type" @= "text")
  (textInput # hover # enabled) <> (textInput # focus # enabled) ? simpleBorder
  (textInput # disabled) ? disableBackground

fontFamilies :: Resources -> Css
fontFamilies Resources{..} = do
  makeFontFace "Roboto" robotoRegularUrl
  makeFontFace "Roboto-Bold" robotoBoldUrl
  makeFontFace "Roboto-Black" robotoBlackUrl
  makeFontFace "Roboto-Medium" robotoMediumUrl
  where
    makeFontFace fontName fontUrl = fontFace $ do
      fontFamily [fontName] []
      fontFaceSrc [FontFaceSrcUrl fontUrl (Just TrueType)]
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
    makeFontFace ffName w urls = fontFace $ do
      fontFamily [ffName] []
      fontStyle normal
      fontFaceSrc [FontFaceSrcUrl ffUrl (Just format)
        | ffUrl    <- urls,
          format <- [EmbeddedOpenType, SVG, TrueType, WOFF, WOFF2]]
      fontWeight $ weight w

mnemonicWidgetCss :: Css
mnemonicWidgetCss = do
  ".mnemonic-word-dx" ? do
    fontFamily ["Roboto-Medium"] []
    fontSize $ pt 18
    textAlign $ alignSide sideLeft
  ".mnemonic-word-md" ? do
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
  ".restore-seed-buttons-wrapper" ? do
    display flex
    flexWrap F.wrap
    marginLeft $ rem (-0.5)
    marginRight $ rem (-0.5)
    justifyContent center
  ".restore-seed-buttons-wrapper button" ? do
    marginLeft $ rem 0.5
    marginRight $ rem 0.5
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
  query M.screen [M.minWidth mobileBreakpoint] $ ".grid3" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1, fr 1]
    gridGap $ rem 1
    width maxContent

settingsCss :: Css
settingsCss = do
  ".initial-options" ? do
    margin (rem 0) auto (rem 0) auto

mnemonicExportCss :: Css
mnemonicExportCss = do
  ".mnemonic-export-text" ? do
    wordBreak breakAll
  ".mnemonic-export-buttons-wrapper" ? do
    display flex
    flexWrap F.wrap
    marginLeft $ rem (-1)
    marginRight $ rem (-1)
    justifyContent center
  ".mnemonic-export-btn-wrapper" ? do
    paddingLeft $ rem 0.5
    paddingRight $ rem 0.5

validateCss :: Css
validateCss = do
  ".validate-error" ? do
    fontSize $ pt 14


toggleSwitchCss :: Css
toggleSwitchCss = do
  ".switch" ? do
    position relative
    display inlineBlock
    width  $ px 60
    height $ px 34
  input # ".switch" ? do
    opacity 0.0
    width $ px 0
    height $ px 0
    verticalAlign vAlignBottom
  ".slider" ? do
    position absolute
    cursor pointer
    top $ px 0
    left $ px 0
    right $ px 0
    bottom $ px 0
    backgroundColor white
    borderStyle solid
    borderWidth $ px 1
    borderColor black

  ".slider" # before ? do
    position absolute
    content $ stringContent mempty
    height $ px 26
    width $ px 26
    left $ px  4
    bottom $ px  3
    backgroundColor black
    verticalAlign middle
  input # checked |+ ".slider" ? do
    backgroundColor grey
    verticalAlign middle
  input # focus |+ ".slider" ? do
    boxShadow $ pure $ bsColor grey $ shadowWithBlur (px 0) (px 0) (px 1)
  input # checked |+ ".slider" # before ? do
    transform $ translateX $ px 26

passwordCss :: Css
passwordCss = do
  ".password-field" ? do
    display flex
    alignItems center
    position relative
    marginBottom $ rem 1.5
  ".eyed-field" ? do
    width $ pct 100
    marginBottom $ rem 0
  ".small-eye" ? do
    position absolute
    display flex
    alignItems center
    right $ rem 0.8
    fontSize $ pt 16
    color hoverColor
    backgroundColor white
  ".small-eye:hover" ? do
    cursor pointer
  ".ask-password-modal" ? do
    position absolute
    top $ px 0
    left $ px 0
    width $ vw 100
    height $ vh 100
    zIndex 2
    backgroundColor white
  ".ask-password-modal-content" ? do
    height $ pct 100
    display flex
    flexDirection column
    justifyContent center
    paddingLeft $ pct 25
    paddingRight $ pct 25
  ".ask-pattern-modal" ? do
    position absolute
    top $ px 0
    left $ px 0
    width $ vw 100
    height $ vh 100
    zIndex 2
    backgroundColor white
    justifyContent center

initialPageCss :: Css
initialPageCss = do
  ".initial-page-content" ? do
    margin auto auto auto auto
  ".initial-page-options" ? do
    display grid
    width maxContent
    margin (rem 0) auto (rem 0) auto
  ".text-pin-code-error" ? do
    color textDanger

balancesPageCss :: Css
balancesPageCss = do
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
    textAlign $ alignSide sideLeft
    display tableCell
    paddingRight $ rem 1
  ".currency-balance" ? do
    display tableCell
    textAlign $ alignSide sideRight
  ".currency-value" ? do
    paddingRight $ rem 0.5
  ".currency-unit" ? do
    paddingRight $ rem 0.5
  ".canvas-container" ? do
    marginLeft $ px 120
    marginTop $ px 50

sendPageCss :: Css
sendPageCss = do
  ".send-page input" ? do
    marginBottom $ rem 1
  ".form-field-errors" ? do
    color red
    textAlign $ alignSide sideLeft
    marginTop $ rem (-0.5)
    marginBottom $ rem 1
  ".send-page-buttons-wrapper" ? do
    display flex
    flexWrap F.wrap
    marginLeft $ rem (-1)
    marginRight $ rem (-1)
  ".send-page-buttons-wrapper button" ? do
    flexGrow 1
    marginLeft $ rem 1
    marginRight $ rem 1
  ".send-page-available-balance" ? do
    paddingBottom $ rem 1
    textAlign $ alignSide sideLeft
  ".button-icon-wrapper" ? do
    paddingLeft $ rem 1
  ".is-invalid input" ? border solid (rem 0.1) red
  ".btn-fee-on" ? do
    important $ backgroundColor gray
    important $ color white
  ".btn-fee-on" `with` hover ? do
    important $ backgroundColor gray
    important $ color white
  ".btn-fee-on" `with` focus ? do
    important $ backgroundColor gray
    important $ color white
  ".lbl-red" ? color red
  ".btn-fee" ? do
    marginLeft $ rem 0.5
    marginRight $ rem 0.5
  ".send-confirm-row" ? do
    textAlign $ alignSide sideLeft
  ".send-confirm-box" ? do
    pure ()

aboutPageCss :: Css
aboutPageCss = do
  ".about-wrapper" ? do
    textAlign center
  ".about-hr-sep" ? do
    border solid (px 3) black
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
  ".about-content-cell-value" ? do
    display tableCell
    let px' = px 5 in padding px' px' px' px'
    textAlign $ alignSide sideLeft
    width $ pct 1
  ".about-distrib" ? do
    paddingTop $ px 45
    fontSize $ pt (if isAndroid then 12 else 18)

networkPageCss :: Css
networkPageCss = do
  ".network-wrapper" ? do
    textAlign center
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
  ".network-title-cur > .select-lang" ? do
    width maxContent
  ".network-hr-sep" ? do
    marginTop $ px 5
    border solid (px 3) black
  ".network-hr-sep-lb" ? do
    border solid (px 1) black
  ".network-hr-sep-nomargin" ? do
    border none none none
    marginTop $ em 0.5
    marginBottom $ em 0.5
  ".network-name" ? do
    display flex
    width $ pct 100
    fontWeight bold
  ".network-name-txt" ? do
    textAlign $ alignSide sideLeft
    wordBreak breakAll
    paddingRight $ em 0.5
  ".network-name-edit" ? do
    display inlineBlock
    float floatRight
    fontWeight bold
    color "#3F7FBF"
  ".network-edit-btn" ? do
    width fitContent
    paddingLeft $ em 1.5
    paddingRight $ em 1.5
  ".network-value" ? do
    display inlineBlock
    float floatLeft
    fontWeight bold
  ".network-descr" ? do
    display inlineBlock
    float floatLeft
    fontStyle italic
    fontSizeCustom smaller
    textAlign $ alignSide sideLeft
    width $ pct 100
  ".network-sel-cur-item" ? do
    textAlign center
    cursor pointer
    fontSize $ pt (if isAndroid then 12 else 18)
  ".indexer-online" ? do
    marginRight $ em 0.5
    color green
  ".indexer-offline" ? do
    marginRight $ em 0.5
    color red
  ".indexer-unsync" ? do
    marginRight $ em 0.5
    color orange
  ".net-refresh-btn" ? do
    height $ em 3.8
    verticalAlign vAlignTop
  ".net-btns-3" ? do
    display grid
    width maxContent
    gridGap $ em 0.5
    marginLeft auto
    marginRight auto
  ".net-btns-2" ? do
    display grid
    width maxContent
    gridGap $ em 0.5
    marginLeft auto
    marginRight auto
  query M.screen [M.minWidth tabletBreakpoint] $ ".net-btns-3" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1, fr 1]
    gridGap $ em 0.5
    width maxContent
  query M.screen [M.minWidth tabletBreakpoint] $ ".net-btns-2" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
    gridGap $ em 0.5
    width maxContent

infoPageCss :: Css
infoPageCss = do
  ".info-v-spacer" ? do
    height $ px 25
  ".info-block-value" ? do
    textAlign $ alignSide sideLeft
    let px3  = px 3
        px10 = px 10
        in padding px3 px10 px3 px10
    border solid (px 1) $ rgb 140 140 140
    let px4 = px 4 in borderRadius px4 px4 px4 px4

sharePageCss :: Css
sharePageCss = do
  ".share-v-spacer" ? do
    height $ px 20
  ".qrcode-container" ? do
    justifyContent center
    margin auto auto auto auto
  ".share-qrcode-container" ? do
    width $ px 252
    height $ px 252
    justifyContent center
    let px0 = px 0 in padding px0 px0 px0 $ px 8
    margin (px 0) auto (px 0) auto
  ".share-buttons-wrapper" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
    gridGap $ rem 1
  ".share-block-value" ? do
    --textAlign $ alignSide sideLeft
    textAlign center
    let px3  = px 5
        px10 = px 10
        in padding px3 px10 px3 px10
    border solid (px 1) $ rgb 140 140 140
    let px4 = px 4 in borderRadius px4 px4 px4 px4
    marginBottom $ px 15
  ".share-image-qrcode" ? do
    width $ px 256
    height $ px 256

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
    position sticky
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
    fontSize $ pt 14
    height $ rem 4.2
  "option" ? do
    fontSize $ pt 14
    height $ rem 4.2
  ".select-fiat" ? do
    margin auto auto auto auto
    width $ px 200
  "select" ? do
    color textColor
    borderColor textColor
    backgroundImage $ url "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 8' width='30'><path fill='%23000000' d='M7,0l6,8l6-8'/></svg>"
    backgroundPosition $ placed sideCenter sideRight
    backgroundRepeat noRepeat
  "select:hover" ? do
    cursor pointer
  "select:hover, select:focus" ? do
    color hoverColor
    borderColor hoverColor
    backgroundImage $ url "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 8' width='30'><path fill='%23707070' d='M7,0l6,8l6-8'/></svg>"
    backgroundPosition $ placed sideCenter sideRight
    backgroundRepeat noRepeat

buttonsToggleCss :: Css
buttonsToggleCss = do
  ".button-currency" ? do
    fontSize $ pt 18
    width $ px 200
  ".button-on" ? do
    important $ backgroundColor "#000000"
    important $ color "#ffffff"
  ".button-off" ? do
    important $ backgroundColor "#ffffff"
    important $ color "#000000"
  ".button-not-working" ? do
    visibility hidden
    pointerEvents none

historyPageCss :: Css
historyPageCss = do
  ".history-table-header" ? do
    marginTop (px 0)
    borderTop solid (px 2) black
    borderBottom solid (px 2) black
    display grid
    gridTemplateColumns [fr 2, fr 3, fr 1]
  ".history-amount-header" ? do
    borderRight solid (px 2) black
    display block
  ".history-date-header" ? do
    borderRight solid (px 2) black
    display block
  "history-date" ? do
    marginLeft auto
    marginRight auto
  ".history-status-header" ? do
    display block
  ".history-status-transrefill" ? do
    display flex
    alignItems center
    justifyContent flexEnd
  ".history-status-transwithdraw" ? do
    display flex
    alignItems center
    justifyContent flexEnd
  ".history-unconfirmed" ? do
    color textDanger
  ".history-partially-confirmed" ? do
    color textWarning
  ".history-confirmed" ? do
    color textSuccess
  ".history-table-row" ? do
    fontSize $ px 16
    padding (rem 1.5) (rem 1) (rem 1.5) (rem 1)
    display grid
    gridTemplateColumns [fr 2, fr 3, fr 1]
  ".history-table-row:hover" ? do
    backgroundColor $ rgb 215 215 219
  ".history-table-row:not(:last-child)" ? do
    borderBottom solid (rem 0.1) (rgb 215 215 219)
  ".history-amount-transrefill" ? do
    display flex
    alignItems center
    color textSuccess
    textAlign $ alignSide sideLeft
  ".history-amount-transwithdraw" ? do
    display flex
    alignItems center
    color textDanger
    textAlign $ alignSide sideLeft
  ".history-page-sign-icon" ? do
    fontSize $ pt 7
    paddingRight $ rem 0.3
  ".history-page-status-icon" ? do
    fontSize $ pt 9
  ".history-page-status-text-icon" ? do
    fontSize $ pt 9
    paddingLeft $ rem 0.5

txInfoPageCss :: Css
txInfoPageCss = do
  ".tx-info-page" ? do
    textAlign $ alignSide sideLeft
  ".tx-info-page p" ? do
    marginBottom $ rem 0
  ".tx-info-page-element" ? do
    marginBottom $ rem 1
  ".tx-info-page-expanded" ? do
    wordBreak breakAll
  ".tx-info-page-minified" ? do
    overflow hidden
    textOverflow overflowEllipsis
  ".tx-info-page-outputs-inputs" ? do
    display grid
    gridTemplateColumns [maxContent, fr 1]
    marginBottom $ rem (-1)
  ".tx-info-page-expand-buttton-wrapper" ? do
    cursor pointer
    display inlineBlock
  ".tx-info-page-expand-buttton" ? do
    paddingLeft $ rem 0.5
    paddingRight $ rem 0.5
  ".tx-info-page-copy" ? do
    cursor pointer
  ".tx-info-our-address" ? do
    fontWeight bold

bumpFeePageCss :: Css
bumpFeePageCss = do
  ".bump-fee-page" ? do
    textAlign $ alignSide sideLeft

legoStyles :: Css
legoStyles = do
  ".mb-0" ? (marginBottom $ rem 0)
  ".ml-0" ? (marginLeft   $ rem 0)
  ".mr-0" ? (marginRight  $ rem 0)
  ".mt-0" ? (marginTop    $ rem 0)
  ".m-0"  ? margin (rem 0) (rem 0) (rem 0) (rem 0)
  ".mb-1" ? (marginBottom $ rem 1)
  ".ml-1" ? (marginLeft   $ rem 1)
  ".mr-1" ? (marginRight  $ rem 1)
  ".mt-1" ? (marginTop    $ rem 1)
  ".m-1" ? margin (rem 1) (rem 1) (rem 1) (rem 1)
  ".mlr-1" ? margin (rem 0) (rem 1) (rem 0) (rem 1)
  ".mlr-a" ? do
    marginLeft auto
    marginRight auto
  ".mb-2" ? (marginBottom $ rem 2)
  ".ml-2" ? (marginLeft   $ rem 2)
  ".mr-2" ? (marginRight  $ rem 2)
  ".mt-2" ? (marginTop    $ rem 2)
  ".m-2" ? margin (rem 2) (rem 2) (rem 2) (rem 2)
  ".mt-3" ? (marginTop    $ rem 3)
  ".mr-6" ? (marginRight  $ rem 6)
  ".mb-a" ? marginBottom  auto
  ".ml-a" ? marginLeft    auto
  ".mr-a" ? marginRight   auto
  ".mt-a" ? marginTop     auto
  ".mtb-a" ? (marginTop auto) >> (marginBottom auto)
  ".pb-1" ? (paddingBottom $ rem 1)
  ".pl-1" ? (paddingLeft   $ rem 1)
  ".pr-1" ? (paddingRight  $ rem 1)
  ".pt-1" ? (paddingTop    $ rem 1)
  ".p-1" ? padding (rem 1) (rem 1) (rem 1) (rem 1)
  ".pb-a" ? paddingBottom  auto
  ".pl-a" ? paddingLeft    auto
  ".pr-a" ? paddingRight   auto
  ".pt-a" ? paddingTop     auto
  ".p-a"  ? padding auto auto auto auto
  ".w-80" ? width (pct 80)
  ".w-100" ? width (pct 100)
  ".h-100" ? height (pct 100)
  ".ta-l" ? textAlign (alignSide sideLeft)
  ".ta-r" ? textAlign (alignSide sideRight)
  ".ta-c" ? textAlign center
  ".ta-l-imp" ? important (textAlign (alignSide sideLeft))
  ".ta-r-imp" ? important (textAlign (alignSide sideRight))
  ".ta-c-imp" ? important (textAlign center)
  ".word-break-all" ? wordBreak breakAll
  ".font-bold" ? fontWeight bold
  ".fit-content" ? width fitContent
  ".disp-block" ? display block
  ".overflow-wrap-bw" ? overflowWrap breakWord
  let fillBtnColor cl backCol fontCol = do
        let colorSet = do
              important (backgroundColor backCol)
              important (color fontCol)
        cl ? colorSet
        cl `with` hover ? colorSet
        cl `with` focus ? colorSet
  fillBtnColor ".btn-color-green" green white
  fillBtnColor ".btn-color-red" red white

badgeCss :: Css
badgeCss = do
  ".badge-warning" ? do
    backgroundColor textWarning
  ".badge-danger" ? do
    backgroundColor textDanger
    color white
  ".badge-info" ? do
    backgroundColor textInfo
    color white
  ".badge-info-2" ? do
    backgroundColor textInfo2

receiveCss :: Css
receiveCss = do
  ".receive-qr" ? do
    margin (rem 2) auto (rem 2) auto
  ".receive-qr-andr" ? do
    margin (rem 2) auto (rem 2) auto
  ".receive-adr" ? do
    margin (rem 2) auto (rem 2) auto
    fontSize $ px 16
    fontWeight $ weight 600
  ".label-block" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
    gridGap $ rem 1
  ".qrcode" ? do
    margin (px 0) auto (px 0) auto
  ".receive-buttons-wrapper" ? do
    display flex
    flexWrap F.wrap
    marginLeft $ rem (-1)
    marginRight $ rem (-1)
    justifyContent center
  ".receive-btn-wrapper" ? do
    paddingLeft $ rem 0.5
    paddingRight $ rem 0.5
  ".receive-adr-andr" ? do
    margin (px 20) (px 5) (px 40) (px 5)
    fontSize $ px 16
    fontWeight $ weight 600
  ".button-receive" ? do
    width $ pct 75
    marginLeft auto
    marginRight auto

graphPinCodeCanvasCss :: Css
graphPinCodeCanvasCss = do
  ".graph-pin-code-canvas" ? do
    position relative
    backgroundColor $ rgb 240 240 240
    border solid (px 1) black
    borderRadius (px 5) (px 5) (px 5) (px 5)
    padding (px 0) (px 0) (px 0) (px 0)
    marginLeft auto
    marginRight auto
    userSelect none
    cursor pointer
    zIndex 3
  ".graph-pin-code-canvas-error" ? do
    position relative
    backgroundColor $ rgb 255 230 230
    border solid (px 1) textDanger
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

linkCss :: Css
linkCss = do
  ".link" ? do
    color textColor
    cursor pointer
    textDecoration underline
  ".link:hover, .link:focus" ? do
    color hoverColor

testnetDisclaimerCss :: Css
testnetDisclaimerCss = do
  ".testnet-disclaimer-label" ? do
    marginBottom $ rem 1
  ".testnet-disclaimer-text" ? do
    marginBottom $ rem 1
