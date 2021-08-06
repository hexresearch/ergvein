{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.View.Erg(
    TxInfo(..)
  , TxView(..)
  , TxDetailedView(..)
) where

import Data.Text (Text)
import Data.Word
import Data.ByteString (ByteString)

import Ergvein.Core.Transaction.View.Common
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

-- Data needed to display the transaction on the screen.
data TxInfo = TxInfo {
    txInfo'blockHash             :: Maybe ByteString -- The hash of a block that this tx was included in. Nothing means that this tx is unconfirmed.
  , txInfo'tx                    :: ErgTx -- Transaction itself.
  , txInfo'amount                :: Money -- Transaction amount.
  , txInfo'hasUnconfirmedParents :: Bool -- Flag indicating the presence of unconfirmed parent transactions.
  , txInfo'outStatuses           :: [TransOutputType] -- Status of transaction outputs (whether they were spent or not) in the same order they are in the transaction.
  , txInfo'spentOutputs          :: [ErgoBox] -- Outputs that were spent by this transaction.
  , txInfo'conflictingTxs        :: [TxId] -- Transactions that conflict with this transaction (e.g. double spending transactions).
  , txInfo'replacedTxs           :: [TxId] -- Transactions that were replaced by this transaction.
  , txInfo'possReplacedTxs       :: (Bool, [TxId]) -- Transactions that may have been replaced by this transaction or that are replacing this transaction. If the first element of the pair is True then this tx is replacing txs listed in the second element of the pair, otherwise it is replaced by given txs.
} deriving (Show)

-- Data needed to display the transaction in compact form on the transaction history page.
data TxView = TxView {
    txView'amount             :: Money -- Transaction amount.
  , txView'prevAmount         :: Maybe Money -- Wallet balance before receiving this transaction.
  , txView'time               :: TxTime -- Time when the transaction was received.
  , txView'inOut              :: TransType -- Flag indicating an incoming or outgoing transaction.
  , txView'detailedView       :: TxDetailedView -- This field is required for redirects to the tx info page.
  , txView'confirmationStatus :: TxConfirmationStatus -- Confirmation status of the transaction.
} deriving (Show)

-- Data needed to display transaction details on the transaction info page.
data TxDetailedView = TxDetailedView {
    txDetailedView'txId                :: Text
  , txDetailedView'tx                  :: ErgTx
  , txDetailedView'label               :: Maybe Text
  , txDetailedView'explorerUrl         :: Text
  , txDetailedView'fee                 :: Maybe Money
  , txDetailedView'conflictingTxs      :: [TxId]
  , txDetailedView'replacedTxs         :: [TxId]
  , txDetailedView'possiblyReplacedTxs :: (Bool, [TxId])
  , txDetailedView'confirmations       :: Word64
  , txDetailedView'block               :: Maybe (Text, Text)
  , txDetailedView'outputs             :: [(Maybe Text, Money, TransOutputType, Bool)]
  , txDetailedView'inputs              :: [(Maybe Text, Money)]
} deriving (Show)
