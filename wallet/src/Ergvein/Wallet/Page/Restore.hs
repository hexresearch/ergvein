module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Filters.Btc
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper
import Ergvein.Wallet.Worker.Node

restorePage :: MonadFront t m =>  m ()
restorePage = wrapperSimple True $ do
  el "h4" $ text "Here we restore you wallet"
