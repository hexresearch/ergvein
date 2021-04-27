{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.QRCode(
    qrCodeWidget
  , qrCodeWidgetWithData
  ) where

import Codec.QRCode

import Ergvein.Text
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import qualified Data.Vector.Unboxed as UV

qrCodeWidget :: MonadFrontBase t m => Text -> m (Element EventResult GhcjsDomSpace t, CanvasOptions)
qrCodeWidget txt = divClass "qrcode-container" $ mdo
  let (qrCodeData, qrCodeSize) = qrcPerCanvas qrData
      cOpts = CanvasOptions qrCodeSize qrCodeSize "qrcode" "qrcode"
      qrData = qrGen txt
  canvasEl <- createCanvas cOpts
  rawJSCall (_element_raw canvasEl) $ drawGridT qrCodeSize qrCodeSize qrCodeData GridStrokeWhite
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
      resolutionRate = 10
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
