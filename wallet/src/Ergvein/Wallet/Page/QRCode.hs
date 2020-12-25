{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.QRCode(
    qrCodeWidget
  , qrCodeWidgetWithData
  ) where

import Codec.QRCode

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import qualified Data.Vector.Unboxed as UV

qrCodeWidget :: MonadFrontBase t m => Int -> Int -> Text -> m (Element EventResult GhcjsDomSpace t, CanvasOptions)
qrCodeWidget canvasH canvasW text = elAttr "div" attrs $ mdo
    --divClass "test" $ text $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    canvasEl <- createCanvas cOpts
    rawJSCall (_element_raw canvasEl) $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    pure (canvasEl, cOpts)
    where
      attrs = [
          ("class", "qrcode-container")
        , ("style", "height:" <> showt (canvasH + 4) <> "px;width:" <> showt (canvasW + 4) <> "px;padding:2px;")
        ]
      cOpts = CanvasOptions canvasW canvasH "qrcode" "qrcode"
      qrData = qrGen text

qrCodeWidgetWithData :: MonadFrontBase t m => Int -> Int -> Text -> m (Dynamic t (Maybe Text))
qrCodeWidgetWithData canvasH canvasW text = do
  buildE <- getPostBuild
  (canvasEl, cOpts) <- qrCodeWidget canvasH canvasW text
  dataE <- performEvent $ ffor buildE $ const $ rawGetCanvasJpeg (_element_raw canvasEl) cOpts
  holdDyn Nothing dataE

qrGen :: Text -> Maybe QRImage
qrGen t = encodeText (defaultQRCodeOptions L) Utf8WithoutECI $ t

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
