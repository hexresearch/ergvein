module Sepulcas.TimeZone(
    getGetTimeZone
  ) where

import Data.Time (TimeZone)
import Sepulcas.Monad
import Sepulcas.Native

getGetTimeZone :: (PerformUI t m, PlatformNatives) => Event t () -> m (Event t TimeZone)
getGetTimeZone e = runOnUiThread $ ffor e $ const getTimeZone
