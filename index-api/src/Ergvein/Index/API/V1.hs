module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Types.Currency
import Ergvein.Index.API.Types

type Body a = ReqBody '[JSON] a
type GetResp a = Get '[JSON] a
type PostResp a = Post '[JSON] a


type IndexGetBalance        = "balance" :> QueryParam "pubKeyScriptHash" PubKeyScriptHash :> GetResp BalanceResponse

type IndexGetTxHashHistory  = "history" :> QueryParam "pubKeyScriptHash" PubKeyScriptHash :> GetResp TxHashHistoryResponse

type IndexGetTxMerkleProof  = "merkle" 
    :> QueryParam "pubKeyScriptHash" PubKeyScriptHash :> QueryParam "blockHeight" BlockHeight 
    :> GetResp TxHashHistoryResponse

type IndexGetTxHexView      = "view" :> Body [TxId] :> PostResp TxHexViewResponse

type IndexGetTxFeeHistogram = "fee" :> "histogram" :> GetResp TxBroadcastResponse

type IndexTxBroadcast       = "broadcast" :> Body TxHexView :> PostResp TxBroadcastResponse
    
data IndexApi route = IndexApi
    { indexGetBalance        :: route :- IndexGetBalance
    , indexGetTxHashHistory  :: route :- IndexGetTxHashHistory
    , indexGetTxMerkleProof  :: route :- IndexGetTxMerkleProof
    , indexGetTxHexView      :: route :- IndexGetTxHexView
    , indexGetTxFeeHistogram :: route :- IndexGetTxFeeHistogram
    , indexTxBroadcast       :: route :- IndexTxBroadcast  
    } deriving Generic