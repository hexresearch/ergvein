module Sepulcas.OpenUrl(
    openOpenUrl
  ) where

import Control.Monad.IO.Unlift
import Data.Text (Text)
import Sepulcas.Monad
import Sepulcas.Native

openOpenUrl :: (PerformEvent t m, TriggerEvent t m, MonadHasUI m, MonadUnliftIO (Performable m), PlatformNatives) => Event t Text -> m (Event t ())
openOpenUrl e = runOnUiThread $ openUrl <$> e
