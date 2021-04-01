module Sepulcas.Style.Embed where

import Data.FileEmbed
import Data.ByteString (ByteString)

milligramCss :: ByteString
milligramCss = $(embedFile "static/css/milligram.min.css")

tooltipCss :: ByteString
tooltipCss = $(embedFile "static/css/tooltip.css")

fontawesomeCss :: ByteString
fontawesomeCss = $(embedFile "static/css/fontawesome.min.css")

robotBlack, robotoBold, robotoMedium, robotoRegular :: ByteString
robotBlack = $(embedFile "static/assets/font/Roboto-Black.ttf")
robotoBold = $(embedFile "static/assets/font/Roboto-Bold.ttf")
robotoMedium = $(embedFile "static/assets/font/Roboto-Medium.ttf")
robotoRegular = $(embedFile "static/assets/font/Roboto-Regular.ttf")

fabrands400eot, fabrands400svg, fabrands400ttf, fabrands400woff, fabrands400woff2 :: ByteString
fabrands400eot = $(embedFile "static/assets/font/fa-brands-400.eot")
fabrands400svg = $(embedFile "static/assets/font/fa-brands-400.svg")
fabrands400ttf = $(embedFile "static/assets/font/fa-brands-400.ttf")
fabrands400woff = $(embedFile "static/assets/font/fa-brands-400.woff")
fabrands400woff2 = $(embedFile "static/assets/font/fa-brands-400.woff2")

faregular400eot, faregular400svg, faregular400ttf, faregular400woff, faregular400woff2 :: ByteString
faregular400eot = $(embedFile "static/assets/font/fa-regular-400.eot")
faregular400svg = $(embedFile "static/assets/font/fa-regular-400.svg")
faregular400ttf = $(embedFile "static/assets/font/fa-regular-400.ttf")
faregular400woff = $(embedFile "static/assets/font/fa-regular-400.woff")
faregular400woff2 = $(embedFile "static/assets/font/fa-regular-400.woff2")

fasolid900eot, fasolid900svg, fasolid900ttf, fasolid900woff, fasolid900woff2 :: ByteString
fasolid900eot = $(embedFile "static/assets/font/fa-solid-900.eot")
fasolid900svg = $(embedFile "static/assets/font/fa-solid-900.svg")
fasolid900ttf = $(embedFile "static/assets/font/fa-solid-900.ttf")
fasolid900woff = $(embedFile "static/assets/font/fa-solid-900.woff")
fasolid900woff2 = $(embedFile "static/assets/font/fa-solid-900.woff2")
