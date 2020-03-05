module Ergvein.Wallet.Camera(
    openCamara
  ) where

import Data.Text (Text)
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native

openCamara :: MonadFrontBase t m => Event t Text -> m (Event t Text)
openCamara e = runOnUiThread $ ffor e $ \str -> do
  cameraWork str
  pure str
