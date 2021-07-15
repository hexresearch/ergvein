{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Get.Erg(
    getTransactionsErgMock
) where

import Data.Time
import Data.Word

import Ergvein.Core.Transaction.View.Common
import Ergvein.Core.Transaction.View.Erg
import Ergvein.Core.Wallet.Monad
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

mockErgTxVeiw :: TxView
mockErgTxVeiw = TxView {
    txView'amount = Money ERGO 1000000
  , txView'prevAmount = Just $ Money ERGO 0
  , txView'time = TxTime $ Just $ parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-6-02"
  , txView'inOut = TransRefill
  , txView'detailedView = mockErgTxDetailedView
  , txView'confirmationStatus = TransConfirmed
}

mockErgTxDetailedView :: TxDetailedView
mockErgTxDetailedView = TxDetailedView {
    txDetailedView'txId = "7b6668423e5f1fa9a7c37006f43c0e82af5cc78bd5bad7a8e3e897787de246c3"
  , txDetailedView'tx = ErgTx mockErgoTx Nothing
  , txDetailedView'label = Nothing
  , txDetailedView'explorerUrl = "https://testnet.ergoplatform.com/en/transactions/7b6668423e5f1fa9a7c37006f43c0e82af5cc78bd5bad7a8e3e897787de246c3"
  , txDetailedView'fee = Just $ Money ERGO 0
  , txDetailedView'conflictingTxs = []
  , txDetailedView'replacedTxs = []
  , txDetailedView'possiblyReplacedTxs = (True, [])
  , txDetailedView'confirmations = 8393
  , txDetailedView'block = Just ("https://testnet.ergoplatform.com/en/blocks/e6d1244e2629719638a6409d0233fe376bf5afc3f553475517239ac6776e08aa", "e6d1244e2629719638a6409d0233fe376bf5afc3f553475517239ac6776e08aa")
  , txDetailedView'outputs = []
  , txDetailedView'inputs = []
}

getTransactionsErgMock :: MonadWallet t m => m (Dynamic t [TxView], Dynamic t Word64)
getTransactionsErgMock = do
  pure (constDyn [mockErgTxVeiw], constDyn 612345)
