module Ergvein.Index.API.Types where

import GHC.Generics
import Ergvein.Aeson
import Ergvein.Types.Currency

-- Balance
data BalanceRequest = BalanceRequest
    { balReqCurrency         :: !Currency
    , balReqPubKeyScriptHash :: !PubKeyScriptHash
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "balReq") ''BalanceRequest)

data BalanceResponse = BalanceResponse
    { balRespConfirmed   :: !MoneyUnit
    , balRespUnconfirmed :: !MoneyUnit
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "balResp") ''BalanceResponse)

-- History
data TxHashHistoryRequest = TxHashHistoryRequest
    { historyReqCurrency         :: !Currency
    , historyReqPubKeyScriptHash :: !PubKeyScriptHash
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "historyReq") ''TxHashHistoryRequest)

data TxHashHistoryItem = TxHashHistoryItem
    { historyItemTxHash      :: !TxHash
    , historyItemBlockHeight :: !BlockHeight
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "historyItem") ''TxHashHistoryItem)

type TxHashHistoryResponse = [TxHashHistoryItem]

-- Merkle
data TxMerkleProofRequest = TxMerkleProofRequest
    { merkleReqCurrency         :: !Currency
    , merkleReqPubKeyScriptHash :: !PubKeyScriptHash
    , merkleReqBlockHeight      :: !BlockHeight
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "merkleReq") ''TxMerkleProofRequest)

data TxMerkleProofItem = TxMerkleProofItem
    { merkleItemTxMerkleProof :: !TxMerkleProof
    , merkleItemTxBlockIndex  :: !TxBlockIndex
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "merkleItem") ''TxMerkleProofItem)

data TxMerkleProofResponse = TxMerkleProof

--View
data TxHexViewRequest = TxHexViewRequest
    { viewReqCurrency :: !Currency
    , viewReqTxIds    :: ![TxId]
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "viewReq") ''TxHexViewRequest)

type TxHexViewResponse = [TxHexView]

--Fee
data TxFeeHistogramRequest = TxFeeHistogramRequest
    { feeHistogramReqCurrency :: !Currency
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "feeHistogramReq") ''TxFeeHistogramRequest)

data TxFeeHistogramItem = TxFeeHistogramItem
    { feeHistogramItemTxFee :: !TxFee
    , feeHistogramItemTxAmount              :: !Word
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "feeHistogramItem") ''TxFeeHistogramItem)

type TxFeeHistogramResponse = [TxFeeHistogramItem]

--Broadcast
data TxBroadcastRequest = TxBroadcastRequest
    { txBroadcastReqCurrency  :: !Currency
    , txBroadcastReqTxHexView :: TxHexView
    } deriving (Generic)
$(deriveJSON (aesonOptionsStripPrefix "txBroadcastReq") ''TxBroadcastRequest)

type TxBroadcastResponse = TxHash

