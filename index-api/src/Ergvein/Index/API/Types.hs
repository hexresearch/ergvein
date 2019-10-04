module Ergvein.Index.API.Types where

import Data.SortedList
import Ergvein.Aeson
import Ergvein.Types.Currency

data BalanceResponse = BalanceResponse
    { balRespConfirmed   :: !MoneyUnit
    , balRespUnconfirmed :: !MoneyUnit
    }
$(deriveJSON (aesonOptionsStripPrefix "balResp") ''BalanceResponse)

data TxHashHistoryItem = TxHashHistoryItem
    { txHash      :: !TxHash
    , blockHeight :: !BlockHeight
    }
$(deriveJSON (aesonOptionsStripPrefix "txHashRespItem") ''TxHashHistoryItem)

type TxHashHistoryResponse = [TxHashHistoryItem]

data TxMerkleProofItem = TxMerkleProofItem
    { txMerkleProof :: !TxMerkleProof
    , txBlockIndex  :: !TxBlockIndex
    }
$(deriveJSON (aesonOptionsStripPrefix "txMerkleProofRespItem") ''TxMerkleProofItem)

data TxMerkleProofResponse = TxMerkleProof

type TxHexViewResponse = [TxHexView]

type TxBroadcastResponse = TxHash

data TxFeeHistogramItem = TxFeeHistogramItem
    { txFee     :: !TxFee
    , txAmount  :: !Word
    }
$(deriveJSON (aesonOptionsStripPrefix "txFeeHistogramItem") ''TxFeeHistogramItem)

type TxFeeHistogramResponse = [TxFeeHistogramItem]