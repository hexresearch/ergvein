module Ergvein.Wallet.Page.BumpFee(
    bumpFeePage
  ) where

import Data.Word (Word64)

import Ergvein.Types.Currency (Currency)
import Ergvein.Wallet.Localization
import Ergvein.Wallet.Monad

bumpFeePage :: MonadFront t m => Currency -> TransactionView -> Maybe (BTCFeeMode, Word64) -> m ()
