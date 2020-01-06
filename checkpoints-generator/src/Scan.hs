module Scan where

import qualified Network.Bitcoin.Api.Client as BitcoinApi

scan :: String -> Int -> String -> String -> IO ()
scan host port user password = do
  pure ()