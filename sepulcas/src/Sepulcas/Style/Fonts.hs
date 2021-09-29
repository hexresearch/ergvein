module Sepulcas.Style.Fonts(
    Fonts(..)
  , embedFonts
  , fontStyles
  ) where

import Clay
import Data.Text (Text)
import Language.Javascript.JSaddle hiding ((#))
import Sepulcas.Embed
import Sepulcas.Style.Embed

data Fonts = Fonts {
  robotoBlackUrl           :: !Text
, robotoBoldUrl            :: !Text
, robotoMediumUrl          :: !Text
, robotoRegularUrl         :: !Text
, materialIconsUrl         :: !Text
, materialIconsRoundUrl    :: !Text
, materialIconsOutlinedUrl :: !Text
, materialIconsSharpUrl    :: !Text
, materialIconsTwoToneUrl  :: !Text
}

embedFonts :: MonadJSM m => m Fonts
embedFonts = Fonts
  <$> createObjectURL robotoBlack
  <*> createObjectURL robotoBold
  <*> createObjectURL robotoMedium
  <*> createObjectURL robotoRegular
  <*> createObjectURL materialIcons
  <*> createObjectURL materialIconsRound
  <*> createObjectURL materialIconsOutlined
  <*> createObjectURL materialIconsSharp
  <*> createObjectURL materialIconsTwoTone

fontStyles :: Fonts -> Css
fontStyles r = do
  robotoFontFamilies r
  materialIconsFontFamilies r

makeFontFace :: [Text] -> Text -> FontFaceFormat -> Maybe FontStyle -> Css
makeFontFace fontNames fontUrl fontFaceFormat mFontStyle = fontFace $ do
  fontFamily fontNames []
  fontFaceSrc [FontFaceSrcUrl fontUrl (Just fontFaceFormat)]
  fontWeight $ weight 400
  case mFontStyle of
    Just fStyle -> fontStyle fStyle
    Nothing -> pure ()

robotoFontFamilies :: Fonts -> Css
robotoFontFamilies Fonts{..} = do
  makeFontFace ["Roboto"] robotoRegularUrl TrueType Nothing
  makeFontFace ["Roboto-Bold"] robotoBoldUrl TrueType Nothing
  makeFontFace ["Roboto-Black"] robotoBlackUrl TrueType Nothing
  makeFontFace ["Roboto-Medium"] robotoMediumUrl TrueType Nothing

materialIconsFontFamilies :: Fonts -> Css
materialIconsFontFamilies Fonts{..} = do
  makeFontFace ["Material Icons"] materialIconsUrl WOFF2 (Just normal)
  makeFontFace ["Material Icons Round"] materialIconsRoundUrl WOFF2 (Just normal)
  makeFontFace ["Material Icons Outlined"] materialIconsOutlinedUrl WOFF2 (Just normal)
  makeFontFace ["Material Icons Sharp"] materialIconsSharpUrl WOFF2 (Just normal)
  makeFontFace ["Material Icons Two Tone"] materialIconsTwoToneUrl WOFF2 (Just normal)
