module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Types.Currency
import Ergvein.Index.API.Types

type Body a = ReqBody '[JSON] a
type PostResp a = Post '[JSON] a


type IndexGetBalance        = "balance"   :> Body BalanceRequest :> PostResp BalanceResponse

type IndexGetTxHashHistory  = "history"   :> Body TxHashHistoryRequest :> PostResp TxHashHistoryResponse

type IndexGetTxMerkleProof  = "merkle"    :> Body TxMerkleProofRequest :> PostResp TxMerkleProofResponse

type IndexGetTxHexView      = "view"      :> Body TxHexViewRequest :> PostResp TxHexViewResponse

type IndexGetTxFeeHistogram = "fee"       :> "histogram" :> Body TxFeeHistogramRequest :> PostResp TxFeeHistogramResponse

type IndexTxBroadcast       = "broadcast" :> Body TxBroadcastRequest :> PostResp TxBroadcastResponse
    
data IndexApi route = IndexApi
    { indexGetBalance        :: route :- IndexGetBalance
    , indexGetTxHashHistory  :: route :- IndexGetTxHashHistory
    , indexGetTxMerkleProof  :: route :- IndexGetTxMerkleProof
    , indexGetTxHexView      :: route :- IndexGetTxHexView
    , indexGetTxFeeHistogram :: route :- IndexGetTxFeeHistogram
    , indexTxBroadcast       :: route :- IndexTxBroadcast  
    } deriving Generic