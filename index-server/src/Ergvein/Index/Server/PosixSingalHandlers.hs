module Ergvein.Index.Server.PosixSingalHandlers where

import Control.Immortal
import System.Posix.Signals
import System.Exit
import Ergvein.Index.Server.Environment
import Database.LevelDB.Internal

onSigTERMHandler :: ServerEnv -> [Thread] -> IO Handler
onSigTERMHandler env workerTreads = do
  installHandler sigTERM (Catch action) Nothing
  where
   action = do
    sequenceA $ mortalize <$> workerTreads
    unsafeClose $ envLevelDBContext env
    sequence_ $ wait <$> workerTreads