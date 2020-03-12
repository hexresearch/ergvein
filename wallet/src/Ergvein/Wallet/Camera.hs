module Ergvein.Wallet.Camera(
    openCamara
  , getResultCamara
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

getResultCamara :: MonadFrontBase t m => Event t () -> m (Event t Text)
getResultCamara e = runOnUiThread $ ffor e $ const cameraGetResult
