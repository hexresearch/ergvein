module Ergvein.Core.TimeZone(
    performTimeZone
  ) where

import Data.Time (TimeZone)
import Reflex
import Reflex.Main.Thread
import Sepulcas.Native

performTimeZone :: (PerformMain t m, PlatformNatives) => Event t () -> m (Event t TimeZone)
performTimeZone e = runOnMainThread $ ffor e $ const getTimeZone
