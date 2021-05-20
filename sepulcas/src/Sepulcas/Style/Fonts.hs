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

embedFonts :: MonadJSM m => m Fonts
embedFonts = Fonts
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

fontStyles :: Fonts -> Css
fontStyles r = do
  fontFamilies r
  faFontFamilies r

fontFamilies :: Fonts -> Css
fontFamilies Fonts{..} = do
  makeFontFace "Roboto" robotoRegularUrl
  makeFontFace "Roboto-Bold" robotoBoldUrl
  makeFontFace "Roboto-Black" robotoBlackUrl
  makeFontFace "Roboto-Medium" robotoMediumUrl
  where
    makeFontFace fontName fontUrl = fontFace $ do
      fontFamily [fontName] []
      fontFaceSrc [FontFaceSrcUrl fontUrl (Just TrueType)]
      fontWeight $ weight 400

faFontFamilies :: Fonts -> Css
faFontFamilies Fonts{..} = do
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
