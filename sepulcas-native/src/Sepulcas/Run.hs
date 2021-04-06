-- | The module overrides standart reflex-dom widget to get access to low level
-- Android callbacks.
module Sepulcas.Run(
    PlatformRun(..)
  ) where

import Sepulcas.Run.Callbacks
import Language.Javascript.JSaddle.Types

class PlatformRun where
  run :: (RunCallbacks -> JSM ()) -> IO ()
