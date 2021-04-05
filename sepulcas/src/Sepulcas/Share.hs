module Sepulcas.Share(
    shareShareUrl
  , shareShareQR
  ) where

import Control.Monad.IO.Unlift
import Data.Text (Text)
import Sepulcas.Monad
import Sepulcas.Native

shareShareUrl :: (PerformUI t m, PlatformNatives) => Event t Text -> m (Event t Text)
shareShareUrl e = runOnUiThread $ ffor e $ \str -> do
  shareUrl str
  pure str

shareShareQR :: (PerformUI t m, PlatformNatives) => Event t (Text, Text) -> m (Event t ())
shareShareQR e = runOnUiThread $ uncurry nativeShareJpeg <$> e
