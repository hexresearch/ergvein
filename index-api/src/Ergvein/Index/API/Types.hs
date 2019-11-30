module Ergvein.Index.API.Types where

import Ergvein.Aeson
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Types.Block
import GHC.Generics
import Data.Word

-- Balance
data BalanceRequest = BalanceRequest
    { balReqCurrency         :: !Currency
    , balReqPubKeyScriptHash :: !PubKeyScriptHash
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "balReq") ''BalanceRequest)

data BalanceResponse = BalanceResponse
    { balRespConfirmed   :: !MoneyUnit
    , balRespUnconfirmed :: !MoneyUnit
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "balResp") ''BalanceResponse)

-- History
data TxHashHistoryRequest = TxHashHistoryRequest
    { historyReqCurrency         :: !Currency
    , historyReqPubKeyScriptHash :: !PubKeyScriptHash
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "historyReq") ''TxHashHistoryRequest)

data TxHashHistoryItem = TxHashHistoryItem
    { historyItemTxHash      :: !TxHash
    , historyItemBlockHeight :: !BlockHeight
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "historyItem") ''TxHashHistoryItem)

type TxHashHistoryResponse = [TxHashHistoryItem]

-- headers
data BlockHeadersRequest = BlockHeadersRequest
    { headersReqCurrency         :: !Currency
    , headersReqStartIndex       :: !BlockHeight
    , headersReqAmount           :: !Word64
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "headersReq") ''BlockHeadersRequest)

type BlockHeadersResponse = [BlockHeaderHexView]

-- Merkle
data TxMerkleProofRequest = TxMerkleProofRequest
    { merkleReqCurrency         :: !Currency
    , merkleReqPubKeyScriptHash :: !PubKeyScriptHash
    , merkleReqBlockHeight      :: !BlockHeight
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "merkleReq") ''TxMerkleProofRequest)

data TxMerkleProofResponse = TxMerkleProofResponse
    { merkleItemTxMerkleProof :: !TxMerkleProof
    , merkleItemTxBlockIndex  :: !TxBlockIndex
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "merkleResp") ''TxMerkleProofResponse)

--View
data TxHexViewRequest = TxHexViewRequest
    { viewReqCurrency :: !Currency
    , viewReqTxIds    :: ![TxId]
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "viewReq") ''TxHexViewRequest)

type TxHexViewResponse = [TxHexView]

--Fee
data TxFeeHistogramRequest = TxFeeHistogramRequest
    { feeHistogramReqCurrency :: !Currency
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "feeHistogramReq") ''TxFeeHistogramRequest)

data TxFeeHistogramItem = TxFeeHistogramItem
    { feeHistogramItemTxFee    :: !TxFee
    , feeHistogramItemTxAmount :: !Word
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "feeHistogramItem") ''TxFeeHistogramItem)

type TxFeeHistogramResponse = [TxFeeHistogramItem]

--Broadcast
data TxBroadcastRequest = TxBroadcastRequest
    { txBroadcastReqCurrency  :: !Currency
    , txBroadcastReqTxHexView :: !TxHexView
    } deriving (Eq, Show, Generic)
$(deriveJSON (aesonOptionsStripPrefix "txBroadcastReq") ''TxBroadcastRequest)

type TxBroadcastResponse = TxHash
