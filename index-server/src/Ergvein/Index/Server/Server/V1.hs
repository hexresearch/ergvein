module Ergvein.Index.Server.Server.V1 where

import Data.Flat
import Data.List
import Data.Monoid

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Index.Server.Cache.Queries
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.Monad
import Ergvein.Types.Currency

indexServer :: IndexApi AsServerM
indexServer = IndexApi
    { indexGetBalance = indexGetBalanceEndpoint
    , indexGetTxHashHistory = indexGetTxHashHistoryEndpoint
    , indexGetBlockHeaders = indexGetBlockHeadersEndpoint
    , indexGetTxMerkleProof = txMerkleProofEndpoint
    , indexGetTxHexView = txHexViewEndpoint
    , indexGetTxFeeHistogram = txFeeHistogramEndpoint
    , indexTxBroadcast = txBroadcastRequestEndpoint
    }
--Stubs
btcBalance  = BalanceResponse { balRespConfirmed = 1024, balRespUnconfirmed = 2048 }
ergoBalance = BalanceResponse { balRespConfirmed = 2048, balRespUnconfirmed = 4096 }

btcHistory = [TxHashHistoryItem {historyItemTxHash = "0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098", historyItemBlockHeight = 0} ]
ergoHistory = [TxHashHistoryItem {historyItemTxHash = "4c6282be413c6e300a530618b37790be5f286ded758accc2aebd41554a1be308", historyItemBlockHeight = 1} ]

btcProof = TxMerkleProofResponse{ merkleItemTxMerkleProof = [""] , merkleItemTxBlockIndex = 1 }
ergoProof = TxMerkleProofResponse{ merkleItemTxMerkleProof = [""] , merkleItemTxBlockIndex = 2 }

btcView = ["btc"]
ergoView = ["ergo"]

btcHistogram = [ TxFeeHistogramItem { feeHistogramItemTxFee = 3, feeHistogramItemTxAmount = 9 }
               , TxFeeHistogramItem { feeHistogramItemTxFee = 4, feeHistogramItemTxAmount = 16 }
               , TxFeeHistogramItem { feeHistogramItemTxFee = 5, feeHistogramItemTxAmount = 25 }
               ]

ergoHistogram = [ TxFeeHistogramItem { feeHistogramItemTxFee = 4, feeHistogramItemTxAmount = 16 }
                , TxFeeHistogramItem { feeHistogramItemTxFee = 6, feeHistogramItemTxAmount = 36 }
                , TxFeeHistogramItem { feeHistogramItemTxFee = 8, feeHistogramItemTxAmount = 64 }
                ]

btcBroadcastResponse = "0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098"
ergoBroadcastResponse = "4c6282be413c6e300a530618b37790be5f286ded758accc2aebd41554a1be308"

--Endpoints
indexGetBalanceEndpoint :: BalanceRequest -> ServerM BalanceResponse
indexGetBalanceEndpoint req@(BalanceRequest { balReqCurrency = BTC  })  = do
  maybeHistory <- getTxOutHistory $ balReqPubKeyScriptHash req
  let confirmedBalance = case maybeHistory of
        Just history -> getSum $ foldMap (Sum . txoValue) history 
        Nothing      -> 0
  pure $ btcBalance { balRespConfirmed = confirmedBalance }
  where
    txoValue (UTXO txo) = txOutCacheRec'value txo
    txoValue _ = 0

indexGetBalanceEndpoint BalanceRequest { balReqCurrency = ERGO } = pure ergoBalance

indexGetTxHashHistoryEndpoint :: TxHashHistoryRequest -> ServerM TxHashHistoryResponse
indexGetTxHashHistoryEndpoint req@(TxHashHistoryRequest{ historyReqCurrency = BTC }) = do
  maybeHistory <- getTxOutHistory $ historyReqPubKeyScriptHash req
  case maybeHistory of
    Just history -> do
        let uniqueHistoryTxIds = nub . mconcat $ utxoHistoryTxIds <$> history
        txs <- getManyParsedExact $ flat . TxCacheRecKey <$> uniqueHistoryTxIds
        let sortedTxs = sortOn txSorting txs
            historyItems = (\tx -> TxHashHistoryItem (txCacheRec'hash tx) (txCacheRec'blockHeight tx)) <$> sortedTxs
        pure historyItems
    _-> pure []
  where
    utxoHistoryTxIds (UTXO txo)         = [txOutCacheRec'txHash txo]
    utxoHistoryTxIds (STXO (txo, stxo)) = [txOutCacheRec'txHash txo , txInCacheRec'txHash stxo]
    txSorting tx = (txCacheRec'blockHeight tx, txCacheRec'blockIndex  tx)

indexGetTxHashHistoryEndpoint TxHashHistoryRequest { historyReqCurrency = ERGO } = pure ergoHistory

indexGetBlockHeadersEndpoint :: BlockHeadersRequest -> ServerM BlockHeadersResponse
indexGetBlockHeadersEndpoint request = do
    let start = flat $ BlockMetaCacheRecKey (headersReqCurrency request) (headersReqStartIndex request)
        end   = BlockMetaCacheRecKey (headersReqCurrency request) (pred $ headersReqStartIndex request + headersReqAmount request)
    slice <- safeEntrySlice start end
    let blockHeaders = snd <$> slice
    pure blockHeaders

txMerkleProofEndpoint :: TxMerkleProofRequest -> ServerM TxMerkleProofResponse
txMerkleProofEndpoint TxMerkleProofRequest { merkleReqCurrency = BTC }  = pure btcProof
txMerkleProofEndpoint TxMerkleProofRequest { merkleReqCurrency = ERGO } = pure ergoProof

txHexViewEndpoint :: TxHexViewRequest -> ServerM TxHexViewResponse
txHexViewEndpoint TxHexViewRequest { viewReqCurrency = BTC }  = pure btcView
txHexViewEndpoint TxHexViewRequest { viewReqCurrency = ERGO } = pure ergoView

txFeeHistogramEndpoint :: TxFeeHistogramRequest -> ServerM TxFeeHistogramResponse
txFeeHistogramEndpoint TxFeeHistogramRequest { feeHistogramReqCurrency = BTC }  = pure btcHistogram
txFeeHistogramEndpoint TxFeeHistogramRequest { feeHistogramReqCurrency = ERGO } = pure ergoHistogram

txBroadcastRequestEndpoint :: TxBroadcastRequest -> ServerM TxBroadcastResponse
txBroadcastRequestEndpoint TxBroadcastRequest { txBroadcastReqCurrency = BTC }  = pure ergoBroadcastResponse
txBroadcastRequestEndpoint TxBroadcastRequest { txBroadcastReqCurrency = ERGO } = pure ergoBroadcastResponse