module Ergvein.Index.Server.TCPService.Socket where

import Control.Concurrent.STM

readAllTVar :: TChan a -> STM [a]
readAllTVar chan = go []
  where
    go !acc = do
      a <- readTChan chan
      empty <- isEmptyTChan chan
      if empty then pure (a : acc) else go (a : acc)