{-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Ergvein.Core.Restore
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Wrapper

restorePage :: MonadFront t m => m ()
restorePage = do
  restoreFinishedE <- restore renderRestorePage
  void $ nextWidget $
    ffor restoreFinishedE $
      const $
        Retractable
          { retractableNext = balancesPage,
            retractablePrev = Nothing
          }

renderRestorePage :: MonadFront t m => m ()
renderRestorePage = wrapperSimpleLogout True $ do
  restoreStatusWidget BTC
  -- restoreStatusDebugWidget BTC
