-- | Module that contains embedded resources for CSS and other resources
module Ergvein.Wallet.Embed.TH where

import Data.FileEmbed
import Data.ByteString (ByteString)

milligramCss :: ByteString
milligramCss = $(embedFile "static/css/milligram.min.css")

tooltipCss :: ByteString
tooltipCss = $(embedFile "static/css/tooltip.css")

robotBlack, robotoBold, robotoMedium, robotoRegular :: ByteString
robotBlack = $(embedFile "static/assets/font/Roboto-Black.ttf")
robotoBold = $(embedFile "static/assets/font/Roboto-Bold.ttf")
robotoMedium = $(embedFile "static/assets/font/Roboto-Medium.ttf")
robotoRegular = $(embedFile "static/assets/font/Roboto-Regular.ttf")

smallEye :: ByteString
smallEye = $(embedFile "static/img/small_eye.png")

menuIcon :: ByteString
menuIcon = $(embedFile "static/img/menu.png")  
