{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Clay.Selector
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
    marginTop $ px 0
    marginLeft $ px 0
    marginRight $ px 0
    fontFamily ["Roboto"] []
  wrapperCss
  menuCss
  buttonCss
  inputCss
  mnemonicWidgetCss
  validateCss
  passwordCss
  initialPageCss
  balancesPageCss
  loadingWidgetCss
  alertsCss

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
    height $ pct 90
  ".vertical-center" ? do
    margin (px 0) (px 0) (px 0) (px 0)
    position absolute
    top $ pct 50
    translatePctY $ pct (-50)
    width $ pct 90

translatePctY :: Size Percentage -> Css
translatePctY y = prefixed (browsers <> "transform") $ "translateY(" <> value y <> ")"

translatePctX :: Size Percentage -> Css
translatePctX x = prefixed (browsers <> "transform") $ "translateX(" <> value x <> ")"

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
    textAlign $ alignSide sideLeft
  ".mnemonic-word-ix" ? do
    fontSize $ em 0.6
    marginRight $ em 0.25
  ".mnemonic-warn" ? do
    marginTop $ px 30
  ".guess-buttons" ? textAlign center
  ".guess-button" ? do
    marginRight $ px 30
    display inlineBlock
  ".restore-word" ? do
    minWidth $ px 120
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
  ".initial-options" ** button ? do
    width $ pct 80
    marginLeft auto
    marginRight auto
    marginBottom $ px 10

balancesPageCss :: Css
balancesPageCss = do
  ".sync-progress" ? do
    width $ pct 100
    maxWidth $ px 500
    display inlineBlock
    textAlign $ alignSide sideLeft
    fontSize $ pt 14
  ".currency-wrapper" ? do
    textAlign center
    cursor pointer
  ".currency-wrapper:hover" ? do
    color hoverColor
  ".currency-line" ? do
    width $ pct 100
    maxWidth $ px 500
    display inlineBlock
    fontSize $ pt 24
  ".currency-name" ? do
    display inlineBlock
    float floatLeft
  ".currency-balance" ? do
    display inlineBlock
    float floatRight

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
    position fixed
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
