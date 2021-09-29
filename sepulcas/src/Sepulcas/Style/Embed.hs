module Sepulcas.Style.Embed where

import Data.FileEmbed
import Data.ByteString (ByteString)

milligramCss :: ByteString
milligramCss = $(embedFile "static/css/milligram.min.css")

tooltipCss :: ByteString
tooltipCss = $(embedFile "static/css/tooltip.css")

materialIconsCss :: ByteString
materialIconsCss = $(embedFile "static/css/material-icons.css")

robotoBlack, robotoBold, robotoMedium, robotoRegular :: ByteString
robotoBlack = $(embedFile "static/assets/font/roboto/Roboto-Black.ttf")
robotoBold = $(embedFile "static/assets/font/roboto/Roboto-Bold.ttf")
robotoMedium = $(embedFile "static/assets/font/roboto/Roboto-Medium.ttf")
robotoRegular = $(embedFile "static/assets/font/roboto/Roboto-Regular.ttf")

materialIcons, materialIconsRound, materialIconsOutlined, materialIconsSharp, materialIconsTwoTone :: ByteString
materialIcons = $(embedFile "static/assets/font/material-icons/MaterialIcons.woff2")
materialIconsRound = $(embedFile "static/assets/font/material-icons/MaterialIconsRound.woff2")
materialIconsOutlined = $(embedFile "static/assets/font/material-icons/MaterialIconsOutlined.woff2")
materialIconsSharp = $(embedFile "static/assets/font/material-icons/MaterialIconsSharp.woff2")
materialIconsTwoTone = $(embedFile "static/assets/font/material-icons/MaterialIconsTwoTone.woff2")
