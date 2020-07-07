module Ergvein.Wallet.TimeZone(
    getGetTimeZone
  ) where

import Data.Time (TimeZone)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native

getGetTimeZone :: MonadFrontBase t m => Event t () -> m (Event t TimeZone)
getGetTimeZone e = runOnUiThread $ ffor e $ const getTimeZone
