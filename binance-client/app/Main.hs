module Main where

import Binance.Client
import Control.Concurrent
import Control.Monad
import Binance.Client.Types

main :: IO ()
main = forever $ do
  v <- getCurrentPrice BTCUSDT
  print $ "[BTCUSDT][CurrentPrice]: " <> show v
  threadDelay 1000000
