module Ergvein.Wallet.Run.Callbacks(
    RunCallbacks(..)
  , noOpRunCallbacks
  ) where

import Control.Concurrent
import Data.IORef

data RunCallbacks = RunCallbacks {
  runBackCallback :: !(IORef (IO ()))
, runUiCallbacks  :: !(Chan (IO ()))
}

noOpRunCallbacks :: IO RunCallbacks
noOpRunCallbacks = RunCallbacks
  <$> newIORef (pure ())
  <*> newChan
