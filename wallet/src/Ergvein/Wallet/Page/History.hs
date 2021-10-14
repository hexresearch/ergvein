module Ergvein.Wallet.Page.History(
    historyPage
  ) where

import Ergvein.Wallet.Monad

import qualified Ergvein.Wallet.Page.History.Btc as Btc

historyPage :: MonadFront t m => Currency -> m ()
historyPage cur = case cur of
  BTC -> Btc.historyPage
