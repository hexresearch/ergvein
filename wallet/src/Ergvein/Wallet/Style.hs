{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Style(
    compileFrontendCss
  ) where

import Clay
import Clay.Selector
import Clay.Stylesheet
import Control.Monad
import Data.ByteString (ByteString)
import Ergvein.Core
import Language.Javascript.JSaddle hiding ((#))
import Prelude hiding ((**), rem)
import Sepulcas.Native
import Sepulcas.Style

import qualified Clay.Media as M
import qualified Clay.Flexbox as F

compileFrontendCss :: (MonadJSM m, PlatformNatives) => m ByteString
compileFrontendCss = compileStyles frontendCss

frontendCss :: PlatformNatives => Css
frontendCss = do
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
  textCss
  linkCss
  loadingWidgetCss
  mnemonicWidgetCss
  navbarCss
  networkPageCss
  toggleSwitchCss
  dropdownContainerCss
  passwordCss
  badgeCss
  receiveCss
  selectCss
  sendPageCss
  settingsCss
  mnemonicExportCss
  validateCss
  wrapperCss
  testnetDisclaimerCss
  backupPageCss

textColor :: Color
textColor = rgb 0 0 0

lightGrey :: Color
lightGrey = rgb 248 249 250

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

appearanceNone :: Css
appearanceNone = prefixed (browsers <> "appearance") noneValue

margin0 :: Css
margin0 = margin (px 0) (px 0) (px 0) (px 0)

padding0 :: Css
padding0 = padding (px 0) (px 0) (px 0) (px 0)

htmlCss :: Css
htmlCss = do
  html ? do
    margin0
    padding0
    textAlign center
  "input, textarea, select" ? do
    fontFamily ["Roboto"] [sansSerif, monospace]
    fontSize $ pt 14

bodyCss :: Css
bodyCss = body ? do
  margin0
  padding0
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
  ".button.button-outline" # disabled # hover ? color black
  ".button.button-clear" # disabled # hover ? color black
  ".back-button" ? do
    textAlign $ alignSide sideLeft
  ".back-button" ** button ? do
    fontSize $ pt 12
  ".button-small" ? do
    fontSize $ rem 0.8
    height $ rem 2.8
    lineHeight $ rem 2.8
    padding (rem 0) (rem 1.5) (rem 0) (rem 1.5)

inputCss :: Css
inputCss = do
  let simpleBorder = border solid (rem 0.1) black
      disableBackground = backgroundColor disabledColor
  let passInput = input # ("type" @= "password")
  (passInput # hover) <> (passInput # focus) ? simpleBorder
  let textInput = input # ("type" @= "text")
  (textInput # hover # enabled) <> (textInput # focus # enabled) ? simpleBorder
  (textInput # disabled) ? disableBackground
  ("textarea" # hover # enabled) <> ("textarea" # focus # enabled) ? simpleBorder
  ("textarea" # disabled) ? disableBackground

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
  ".mnemonic-warn" ? do
    marginTop $ px 30
  ".mnemonic-verification-container" ? do
    display flex
    flexDirection column
    justifyContent spaceBetween
    borderRadius (px 10) (px 10) (px 10) (px 10)
    borderStyle solid
    borderWidth $ px 1
    borderColor hoverColor
    minHeight $ px 200
    padding (rem 0.5) (rem 1) (rem 0.5) (rem 1)
  ".mnemonic-verification-btn-container" ? do
    display flex
    flexWrap F.wrap
    justifyContent center
    marginLeft $ rem (-0.5)
    marginRight $ rem (-0.5)
  ".mnemonic-verification-btn-container button" ? do
    margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    color black
    textTransform none
    fontSize $ rem 1.6
    fontWeight normal
  ".mnemonic-word-disabled" ? do
    borderStyle dashed
    important $ color transparent

  -- These rules fixes bug on Android when disabled button with focus has no visible border
  ".mnemonic-word-disabled:focus, .mnemonic-word-disabled:hover" ? do
    important $ borderColor black
    borderStyle dashed

  ".mnemonic-verification-error" ? do
    color textDanger
    margin (rem 0) (rem 0.5) (rem 0) (rem 0.5)
  ".restore-seed-input" ? do
    minHeight $ rem 10
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


settingsCss :: Css
settingsCss = do
  ".initial-options" ? do
    margin (rem 0) auto (rem 0) auto
  ".fiat-settings" ? do
    display flex
    justifyContent spaceBetween
    alignItems center
    marginBottom $ rem 1
  ".fiat-settings select, .fiat-settings .toggle-switch" ? do
    marginBottom $ rem 0
  ".fiat-settings select" ? do
    width $ rem 10
  ".fiat-settings-label" ? do
    textAlign $ alignSide sideLeft
    fontSize $ pt 14

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
toggleSwitchCss =
  let
    toggleSwitchHeight = rem 3.8 -- change this to scale switch
    toggleSwitchBorderWidth = px 1
    toggleSwitchFontSize = pt 10
    toggleSwitchWidth = 1.6 *@ toggleSwitchHeight
    toggleSwitchBorderRadius = 0.5 *@ toggleSwitchHeight
    knobSize = 0.75 *@ toggleSwitchHeight
    knobBorderRadius = 0.5 *@ knobSize
    knobMargin = 0.5 *@ (toggleSwitchHeight @-@ knobSize @-@ (2 *@ toggleSwitchBorderWidth))
  in do
    ".toggle-switch" ? do
      display inlineBlock
      position relative
      width toggleSwitchWidth
      height toggleSwitchHeight
      minWidth toggleSwitchWidth
      borderStyle solid
      borderWidth toggleSwitchBorderWidth
      borderColor black
      borderRadius toggleSwitchBorderRadius toggleSwitchBorderRadius toggleSwitchBorderRadius toggleSwitchBorderRadius
      margin (rem 0) (rem 0) (rem 1.5) (rem 0)

    ".toggle-switch:hover" ? do
      borderColor hoverColor
      cursor pointer

    ".toggle-switch input" ? do
      appearanceNone
      outline none (px 0) black -- same as outline: none
      display inlineBlock
      width $ pct 100
      height $ pct 100
      padding0
      margin0

    ".toggle-switch input:hover" ? do
      cursor pointer

    ".toggle-switch input:hover label, .toggle-switch:hover label" ? do
      cursor pointer
      backgroundColor hoverColor

    ".toggle-switch label" ? do
      display flex
      alignItems center
      justifyContent center
      position absolute
      width knobSize
      height knobSize
      top knobMargin
      left knobMargin
      borderRadius knobBorderRadius knobBorderRadius knobBorderRadius knobBorderRadius
      backgroundColor black
      margin0
      transition "left" (sec 0.3) easeOut (sec 0)

    ".toggle-switch label::before" ? do
      fontFamily ["Font Awesome 5 Free"] []
      fontWeight $ weight 900
      color white
    
    ".toggle-switch input:not(:checked) + label::before" ? do
      content $ stringContent "\\f00d"
      fontSize $ 1.1 *@ toggleSwitchFontSize

    ".toggle-switch input:checked + label::before" ? do
      content $ stringContent "\\f00c"
      fontSize toggleSwitchFontSize

    ".toggle-switch input:checked + label" ? do
      left $ toggleSwitchWidth @-@ (knobMargin @+@ knobSize @+@ 2 *@ toggleSwitchBorderWidth)

dropdownContainerCss :: Css
dropdownContainerCss = do
  ".dropdown-header-containter" ? do
    marginBottom $ rem 1.5
  ".dropdown-header" ? do
    display inlineBlock
  ".dropdown-header:hover" ? do
    cursor pointer
    color hoverColor
  ".dropdownContainerHidden" ? do
    display displayNone

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
    right $ rem 1.2
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

balancesPageCss :: PlatformNatives => Css
balancesPageCss = do
  ".currency-content" ? do
    width $ pct 100
  ".currency-row" ? do
    cursor pointer
    paddingBottom $ rem 1
  ".currency-row:hover" ? do
    color hoverColor
  ".currency-row:not(:last-child)" ? do
    borderBottom solid (rem 0.1) (rgb 215 215 219)
  ".currency-details" ? do
    display flex
    justifyContent spaceBetween
    fontSize $ pt (if isAndroid then 18 else 24)
  ".currency-name" ? do
    paddingRight $ rem 1
  ".currency-value" ? do
    paddingRight $ rem 0.5
  ".canvas-container" ? do
    marginLeft $ px 120
    marginTop $ px 50
  ".currency-status" ? do
    textAlign $ alignSide sideLeft

sendPageCss :: Css
sendPageCss = do
  ".send-page" ? do
    textAlign $ alignSide sideLeft
  ".text-input-with-btns-wrapper" ? do
    display flex
    alignItems center
    position relative
  ".text-input-with-btns-android" ? do
    width $ pct 100
    marginBottom $ rem 0
    important $ paddingRight $ rem 10
  ".text-input-with-btns-desktop" ? do
    width $ pct 100
    marginBottom $ rem 0
    important $ paddingRight $ rem 5
  ".text-input-btns" ? do
    position absolute
    display flex
    alignItems center
    right $ rem 0
    fontSize $ pt 16
    marginRight $ rem 1
  ".text-input-btn" ? do
    paddingLeft $ rem 1
    paddingRight $ rem 1
  ".text-input-btn:hover" ? do
    cursor pointer
    color hoverColor
  ".form-field-errors" ? do
    color red
    textAlign $ alignSide sideLeft
    marginTop $ rem 0.5
  ".amount-available-balance" ? do
    marginTop $ rem 0.5
  ".send-page-buttons-wrapper" ? do
    display flex
    flexWrap F.wrap
    marginLeft $ rem (-1)
    marginRight $ rem (-1)
  ".send-page-buttons-wrapper button" ? do
    flexGrow 1
    marginLeft $ rem 1
    marginRight $ rem 1
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
  ".send-page .toggle-switch" ? do
    marginBottom $ rem 0

aboutPageCss :: PlatformNatives => Css
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

networkPageCss :: PlatformNatives => Css
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
  ".history-page" ? do
    display flex
  ".history-table" ? do
    flexGrow 1
  ".history-empty-placeholder" ? do
    alignSelf center
    flexGrow 1
    textAlign center
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
  ".history-date" ? do
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
  ".seed-backup-btn" ? do
    position absolute
    right $ rem 2
    bottom $ rem 1
    backgroundColor $ rgb 255 147 30
    borderColor $ rgb 255 147 30

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
  ".pb-2" ? (paddingBottom $ rem 2)
  ".pl-2" ? (paddingLeft   $ rem 2)
  ".pr-2" ? (paddingRight  $ rem 2)
  ".pt-2" ? (paddingTop    $ rem 2)
  ".p-2" ? padding (rem 2) (rem 2) (rem 2) (rem 2)
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
  ".receive-adr" ? do
    margin (rem 2) auto (rem 2) auto
    fontSize $ px 16
    fontWeight $ weight 600
  ".label-block" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
    gridGap $ rem 1
  ".qrcode" ? do
    width $ pct 100
    maxWidth $ rem 30
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
    padding0
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
    backgroundColor none
    let px' = px 0 in padding px' px' px' px'
    let px' = px 0 in margin px' px' px' px'
    userSelect none
    cursor pointer
    borderTop solid (px 2) $ rgb 90 90 90
    height $ px 2
    zIndex 10

textCss :: Css
textCss = do
  ".text-muted" ? do
    color hoverColor

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

backupPageCss :: Css
backupPageCss = do
  ".backup-page-icon" ? do
    fontSize $ pt 45
