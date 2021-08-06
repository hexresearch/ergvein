{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.QRCode(
    qrCodeWidget
  , qrCodeWidgetWithData
  ) where

import Codec.QRCode

import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import qualified Data.Vector.Unboxed as UV

-- Size of one pixel of QR code
resolutionRate :: Int
resolutionRate = 10

qrCodeWidget :: MonadFrontBase t m => Text -> m (Element EventResult GhcjsDomSpace t, CanvasOptions)
qrCodeWidget txt = divClass "qrcode-container" $ mdo
  let (qrCodeData, qrCodeSize) = qrcPerCanvas qrData
      padding = 4 * resolutionRate -- Padding is equal to 4 pixels of QR code
      canvasSize = qrCodeSize + 2 * padding
      cOpts = CanvasOptions canvasSize canvasSize "qrcode" "qrcode"
      qrData = qrGen txt
  canvasEl <- createCanvas cOpts
  rawJSCall (_element_raw canvasEl) $ drawGridT qrCodeSize qrCodeSize padding qrCodeData GridStrokeWhite
  pure (canvasEl, cOpts)

qrCodeWidgetWithData :: MonadFrontBase t m => Text -> m (Dynamic t (Maybe Text))
qrCodeWidgetWithData txt = do
  buildE <- getPostBuild
  (canvasEl, cOpts) <- qrCodeWidget txt
  dataE <- performEvent $ ffor buildE $ const $ rawGetCanvasJpeg (_element_raw canvasEl) cOpts
  holdDyn Nothing dataE

qrGen :: Text -> Maybe QRImage
qrGen t = encodeText (defaultQRCodeOptions L) Utf8WithoutECI $ t

qrcPerCanvas :: Maybe QRImage -> ([(Maybe Int, Square)], Int)
qrcPerCanvas mqrI = case mqrI of
  Nothing -> ([], 0)
  Just qrI -> (zip qrFillList $ colList qrSize qrCount, qrCount * resolutionRate)
    where
      
      qrFillList = fmap boolfill $ UV.toList $ qrImageData qrI
      qrLen = (UV.length . qrImageData) $ qrI
      qrCount = floor $ sqrt $ ((fromIntegral qrLen) :: Double)
      qrSize = resolutionRate
      boolfill a = if a
        then Just 1
        else Nothing

colList :: Int -> Int -> [Square]
colList width cnt = mconcat $ fmap (\num -> rowList width cnt num) rList
  where
    rList = fmap fromIntegral $ [0 .. (cnt-1)]

rowList :: Int -> Int -> Double -> [Square]
rowList width cnt globN = fmap (\num -> (w*num, w*globN, w, w)) rList
  where
    rList = fmap fromIntegral $ [0 .. (cnt-1)]
    w = fromIntegral width
