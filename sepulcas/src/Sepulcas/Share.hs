module Sepulcas.Share(
    shareShareUrl
  , shareShareQR
  ) where

import Data.Text (Text)
import Sepulcas.Monad

shareShareUrl :: (PerformMain t m, PlatformNatives) => Event t Text -> m (Event t Text)
shareShareUrl e = runOnMainThread $ ffor e $ \str -> do
  shareUrl str
  pure str

shareShareQR :: (PerformMain t m, PlatformNatives) => Event t (Text, Text) -> m (Event t ())
shareShareQR e = runOnMainThread $ uncurry nativeShareJpeg <$> e
