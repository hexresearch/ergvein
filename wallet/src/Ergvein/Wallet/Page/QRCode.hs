module Ergvein.Wallet.Page.QRCode(
    qrCodeWidget
  , qrCodeWidgetWithData
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Canvas

import Reflex.Dom

import Language.Javascript.JSaddle.Evaluate
import Language.Javascript.JSaddle.Monad
import Language.Javascript.JSaddle.Value

import qualified Data.Vector.Unboxed              as UV
import           Codec.QRCode
import           Control.Lens                     (to, (^.))
import           Control.Monad.IO.Class           (liftIO)
import           Data.Maybe (fromMaybe)

qrCodeWidget :: MonadFrontBase t m => Text -> m (Element EventResult GhcjsDomSpace t, CanvasOptions)
qrCodeWidget text = divClass "qrcode-container" $ mdo
    --divClass "test" $ text $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    canvasEl <- createCanvas cOpts
    rawJSCall (_element_raw canvasEl) $ drawGridT canvasW canvasH (qrcPerCanvas qrData canvasW)
    pure (canvasEl, cOpts)
    where
      canvasH = 252
      canvasW = 252
      cOpts = CanvasOptions canvasW canvasH "qrcode" "qrcode"
      qrData = qrGen text

qrCodeWidgetWithData :: MonadFrontBase t m => Text -> m (Dynamic t (Maybe Text))
qrCodeWidgetWithData text = do
  buildE <- getPostBuild
  (canvasEl, cOpts) <- qrCodeWidget text
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
colList width count = mconcat $ fmap (\num -> rowList width count num) rList
  where
    rList = fmap fromIntegral $ [0 .. (count-1)]

rowList :: Int -> Int -> Double -> [Square]
rowList width count globN = fmap (\num -> (w*num,w*globN, w,w)) rList
  where
    rList = fmap fromIntegral $ [0 .. (count-1)]
    w = fromIntegral width
