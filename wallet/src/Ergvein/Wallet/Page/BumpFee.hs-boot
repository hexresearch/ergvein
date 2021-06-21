module Ergvein.Wallet.Page.BumpFee(
    bumpFeePage
  ) where

import Data.Word (Word64)

import Ergvein.Core.Transaction.View.Btc
import Ergvein.Types.Currency (Currency)
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad

bumpFeePage :: MonadFront t m => Currency -> TxView -> Maybe (FeeMode, Word64) -> m ()
