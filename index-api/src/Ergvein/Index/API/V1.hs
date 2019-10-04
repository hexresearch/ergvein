module Ergvein.Index.API.V1 where

import Servant.API
import Servant.API.Generic
import Ergvein.Types.Currency
import Ergvein.Index.API.Types

type Body a = ReqBody '[JSON] a
type GetResp a = Get '[JSON] a
type PostResp a = Post '[JSON] a


type IndexGetBalance = QueryParam "pubKeyScriptHash" PubKeyScriptHash :> GetResp BalanceResponse

type IndexGetTxHashHistory = QueryParam "pubKeyScriptHash" PubKeyScriptHash :> GetResp TxHashHistoryResponse

type IndexGetTxMerkleProof = 
       QueryParam "pubKeyScriptHash" PubKeyScriptHash
    :> QueryParam "blockHeight" BlockHeight
    :> GetResp TxHashHistoryResponse

type IndexGetTxHexView = Body [TxId] :> PostResp TxHexViewResponse


type IndexTxBroadcast = QueryParam "txHexView" TxHexView :> GetResp TxBroadcastResponse

type IndexGetTxFeeHistogram = GetResp TxBroadcastResponse
    
data IndexApi route = IndexApi
    { indexGetBalance        :: route :- IndexGetBalance
    , indexGetTxHashHistory  :: route :- IndexGetTxHashHistory
    , indexGetTxMerkleProof  :: route :- IndexGetTxMerkleProof
    , indexGetTxHexView      :: route :- IndexGetTxHexView
    , indexGetTxFeeHistogram :: route :- IndexGetTxFeeHistogram
    , indexTxBroadcast       :: route :- IndexTxBroadcast  
    }