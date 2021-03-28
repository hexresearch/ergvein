module Ergvein.Wallet.Page.BumpFee(
    bumpFeePage
  ) where

import Data.Word (Word64)

import Ergvein.Types.Currency (Currency)
import Ergvein.Wallet.Localization.Fee (BTCFeeMode)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Transaction.View (TransactionView)

bumpFeePage :: MonadFront t m => Currency -> TransactionView -> Maybe (BTCFeeMode, Word64) -> m ()
