module Sepulcas.OpenUrl(
    openOpenUrl
  ) where

import Data.Text (Text)
import Sepulcas.Monad

openOpenUrl :: (PerformMain t m, PlatformNatives) => Event t Text -> m (Event t ())
openOpenUrl e = runOnMainThread $ openUrl <$> e
