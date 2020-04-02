module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Index.API.Types

type Body a = ReqBody '[JSON] a
type PostResp a = Post '[JSON] a


type IndexGetHeight         = "height"    :> Body HeightRequest :> PostResp HeightResponse

type IndexGetBalance        = "balance"   :> Body BalanceRequest :> PostResp BalanceResponse

type IndexGetTxHashHistory  = "history"   :> Body TxHashHistoryRequest :> PostResp TxHashHistoryResponse

type IndexGetBlockHeaders   = "headers"   :> Body BlockHeadersRequest :> PostResp BlockHeadersResponse

type IndexGetBlockFilters   = "filters"   :> Body BlockFiltersRequest :> PostResp BlockFiltersResponse

type IndexGetTxMerkleProof  = "merkle"    :> Body TxMerkleProofRequest :> PostResp TxMerkleProofResponse

type IndexGetTxHexView      = "view"      :> Body TxHexViewRequest :> PostResp TxHexViewResponse

type IndexGetTxFeeHistogram = "fee"       :> "histogram" :> Body TxFeeHistogramRequest :> PostResp TxFeeHistogramResponse

type IndexTxBroadcast       = "broadcast" :> Body TxBroadcastRequest :> PostResp TxBroadcastResponse

type IndexGetInfo           = "info"      :> PostResp InfoResponse

data IndexApi route = IndexApi
    { indexGetHeight         :: route :- IndexGetHeight
    , indexGetBalance        :: route :- IndexGetBalance
    , indexGetTxHashHistory  :: route :- IndexGetTxHashHistory
    , indexGetBlockHeaders   :: route :- IndexGetBlockHeaders
    , indexGetBlockFilters   :: route :- IndexGetBlockFilters
    , indexGetTxMerkleProof  :: route :- IndexGetTxMerkleProof
    , indexGetTxHexView      :: route :- IndexGetTxHexView
    , indexGetTxFeeHistogram :: route :- IndexGetTxFeeHistogram
    , indexTxBroadcast       :: route :- IndexTxBroadcast
    , indexGetInfo           :: route :- IndexGetInfo
    } deriving Generic
