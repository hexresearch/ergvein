module Sepulcas.OpenUrl(
    openOpenUrl
  ) where

import Control.Monad.IO.Unlift
import Data.Text (Text)
import Sepulcas.Monad
import Sepulcas.Native

openOpenUrl :: (PerformUI t m, PlatformNatives) => Event t Text -> m (Event t ())
openOpenUrl e = runOnUiThread $ openUrl <$> e
