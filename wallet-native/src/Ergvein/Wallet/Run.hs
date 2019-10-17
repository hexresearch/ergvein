-- | The module overrides standart reflex-dom widget to get access to low level
-- Android callbacks.
module Ergvein.Wallet.Run(
    PlatformRun(..)
  ) where

import Ergvein.Wallet.Run.Callbacks
import Language.Javascript.JSaddle.Types

class PlatformRun where
  run :: (RunCallbacks -> JSM ()) -> IO ()
