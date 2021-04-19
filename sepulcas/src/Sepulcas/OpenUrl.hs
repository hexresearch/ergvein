module Sepulcas.OpenUrl(
    openOpenUrl
  ) where

import Control.Monad.IO.Unlift
import Data.Text (Text)
import Sepulcas.Monad
import Sepulcas.Native

openOpenUrl :: (PerformMain t m, PlatformNatives) => Event t Text -> m (Event t ())
openOpenUrl e = runOnMainThread $ openUrl <$> e
