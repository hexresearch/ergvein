{-# OPTIONS_GHC -Wno-type-defaults #-}
module Ergvein.Wallet.Page.QRCode(
    qrCodeWidget
  , qrCodeWidgetWithData
  ) where

import Codec.QRCode

import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import qualified Data.Vector.Unboxed as UV

qrCodeWidget :: MonadFrontBase t m => Text -> Currency -> m (Element EventResult GhcjsDomSpace t, CanvasOptions)
qrCodeWidget addr cur = divClass "qrcode-container" $ mdo
    --divClass "test" $ text $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    canvasEl <- createCanvas cOpts
    rawJSCall (_element_raw canvasEl) $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    pure (canvasEl, cOpts)
    where
      canvasH = 252
      canvasW = 252
      cOpts = CanvasOptions canvasW canvasH "qrcode" "qrcode"
      qrData = qrcGen addr cur

qrCodeWidgetWithData :: MonadFrontBase t m => Text -> Currency -> m (Dynamic t (Maybe Text))
qrCodeWidgetWithData addr cur = do
  buildE <- getPostBuild
  (canvasEl, cOpts) <- qrCodeWidget addr cur
  dataE <- performEvent $ ffor buildE $ const $ rawGetCanvasJpeg (_element_raw canvasEl) cOpts
  holdDyn Nothing dataE

qrcGen :: Text -> Currency -> Maybe QRImage
qrcGen t cur = encodeText (defaultQRCodeOptions L) Utf8WithoutECI $ curprefix cur <> t

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
colList width cnt = mconcat $ fmap (\num -> rowList width cnt num) rList
  where
    rList = fmap fromIntegral $ [0 .. (cnt-1)]

rowList :: Int -> Int -> Double -> [Square]
rowList width cnt globN = fmap (\num -> (w*num,w*globN, w,w)) rList
  where
    rList = fmap fromIntegral $ [0 .. (cnt-1)]
    w = fromIntegral width
