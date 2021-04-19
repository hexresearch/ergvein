module Sepulcas.Camera(
    openCamara
  , getResultCamara
  , waiterResultCamera
  , debugCameraPage
  ) where

import Control.Monad.Fix
import Data.Text (Text)
import Data.Text as T
import Sepulcas.Elements
import Sepulcas.Monad
import Sepulcas.Native
import Reflex.Dom

openCamara :: (PerformMain t m, PlatformNatives) => Event t () -> m (Event t ())
openCamara e = runOnMainThread $ ffor e $ \_ -> do
  cameraWork ""
  pure ()

getResultCamara :: (PerformMain t m, PlatformNatives) => Event t () -> m (Event t Text)
getResultCamara e = runOnMainThread $ ffor e $ const cameraGetResult

waiterResultCamera :: (PerformMain t m, PlatformNatives, MonadFix m) => Event t () -> m (Event t Text)
waiterResultCamera startE = mdo
  resE <- getResultCamara $ leftmost [startE, nextE]
  nextE <- delay 1.0 $ fforMaybe resE $ \v -> case T.null v of
                        True  -> Just ()
                        False -> Nothing
  pure $ fforMaybe resE $ \v -> case T.null v of
          True  -> Nothing
          False -> Just v

debugCameraPage :: (DomBuilder t m, PerformMain t m, MonadLocalized t m, PostBuild t m, MonadHold t m, PlatformNatives, MonadFix m) => m ()
debugCameraPage = do
  cameraE <- outlineButton ("Debug QR scan"::Text)
  openE <- openCamara cameraE
  openGoE <- delay 1.0 openE
  resE <- waiterResultCamera openGoE
  resD <- holdDyn "RESULT" resE
  h4 $ dynText resD
  pure ()
