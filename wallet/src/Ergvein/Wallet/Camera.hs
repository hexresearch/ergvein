module Ergvein.Wallet.Camera(
    openCamara
  , getResultCamara
  , waiterResultCamera
  , debugCameraPage
  ) where

import Data.Text (Text)
import Data.Text as T
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native

openCamara :: MonadFrontBase t m => Event t () -> m (Event t ())
openCamara e = runOnUiThread $ ffor e $ \_ -> do
  cameraWork ""
  pure ()

getResultCamara :: MonadFrontBase t m => Event t () -> m (Event t Text)
getResultCamara e = runOnUiThread $ ffor e $ const cameraGetResult

waiterResultCamera :: MonadFrontBase t m => Event t () -> m (Event t Text)
waiterResultCamera startE = mdo
  resE <- getResultCamara $ leftmost [startE, nextE]
  nextE <- delay 1.0 $ fforMaybe resE $ \v -> case T.null v of
                        True  -> Just ()
                        False -> Nothing
  pure $ fforMaybe resE $ \v -> case T.null v of
          True  -> Nothing
          False -> Just v

debugCameraPage :: MonadFrontBase t m => m ()
debugCameraPage = do
  cameraE <- outlineButton ("Debug QR scan"::Text)
  openE <- openCamara cameraE
  openGoE <- delay 1.0 openE
  resE <- waiterResultCamera openGoE
  resD <- holdDyn "RESULT" resE
  h4 $ dynText resD
  pure ()
