module Ergvein.Crypto.Constants(
    CoinType(..)
  ) where

data CoinType = BTC | BTCTestnet | ERGO | ERGOTestnet
  deriving (Show, Eq)