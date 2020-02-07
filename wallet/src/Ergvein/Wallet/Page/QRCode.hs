module Ergvein.Wallet.Page.QRCode(
  qrCodeWidget
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import Reflex.Dom

import qualified Data.Vector.Unboxed              as UV
import           Codec.QRCode
import           Control.Lens                     (to, (^.))
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)

qrCodeWidget :: MonadFrontBase t m => Text -> Currency -> m ()
qrCodeWidget addr cur = divClass "qrcode-container" $ mdo
    canvasEl <- createCanvas cOpts
    rawJSCall (_element_raw canvasEl) $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    where
      canvasH = 256
      canvasW = 256
      cOpts = CanvasOptions canvasW canvasH "qrcode" "qrcode"
      qrData = qrcGen addr cur

qrcGen :: Text -> Currency -> Maybe QRImage
qrcGen t cur = encodeText (defaultQRCodeOptions L) Utf8WithoutECI $ curprefix <> t
  where
    curprefix :: Text
    curprefix = case cur of
      BTC ->  "bitcoin:"
      ERGO -> "ergo:"

qrcPerCanvas :: Maybe QRImage -> Int -> [(Maybe Int, Square)]
qrcPerCanvas mqrI cW = case mqrI of
  Nothing -> []
  Just qrI -> zip qrFillList $ colList qrSize qrCount
    where
      qrFillList = fmap boolfill $ UV.toList $ qrImageData qrI
      qrLen = (UV.length . qrImageData) $ qrI
      qrCount = floor $ sqrt $ ((fromIntegral qrLen) :: Double)
      qrSize = floor $ (fromIntegral cW) / (fromIntegral qrCount)
      boolfill a = if a
        then Just 1
        else Nothing

colList :: Int -> Int -> [Square]
colList width count = mconcat $ fmap (\num -> rowList width count num) rList
  where
    rList = fmap fromIntegral $ [0 .. (count-1)]

rowList :: Int -> Int -> Double -> [Square]
rowList width count globN = fmap (\num -> (w*num,w*globN, w,w)) rList
  where
    rList = fmap fromIntegral $ [0 .. (count-1)]
    w = fromIntegral width
