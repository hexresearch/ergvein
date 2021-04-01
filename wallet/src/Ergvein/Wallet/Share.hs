module Ergvein.Wallet.Share(
    shareShareUrl
  , shareShareQR
  ) where

import Data.Text (Text)
import Ergvein.Wallet.Monad
import Sepulcas.Native

shareShareUrl :: MonadFrontBase t m => Event t Text -> m (Event t Text)
shareShareUrl e = runOnUiThread $ ffor e $ \str -> do
  shareUrl str
  pure str

shareShareQR :: MonadFrontBase t m => Event t (Text, Text) -> m (Event t ())
shareShareQR e = runOnUiThread $ uncurry nativeShareJpeg <$> e
