module Ergvein.Index.Server.Server.V1 where

import Data.Flat
import Data.List
import Data.Maybe
import Data.Monoid
import Database.Persist.Sql

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Index.Server.Cache.Queries
import Ergvein.Index.Server.Cache.Schema
import Ergvein.Index.Server.DB.Schema
import Ergvein.Index.Server.DB.Monad
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.Monad
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Data.Word

indexServer :: IndexApi AsServerM
indexServer = IndexApi
    { indexGetHeight = indexGetHeightEndpoint
    , indexGetBalance = indexGetBalanceEndpoint
    , indexGetTxHashHistory = indexGetTxHashHistoryEndpoint
    , indexGetBlockHeaders = indexGetBlockHeadersEndpoint
    , indexGetBlockFilters = indexGetBlockFiltersEndpoint
    , indexGetTxMerkleProof = txMerkleProofEndpoint
    , indexGetTxHexView = txHexViewEndpoint
    , indexGetTxFeeHistogram = txFeeHistogramEndpoint
    , indexTxBroadcast = txBroadcastRequestEndpoint
    }
-- Stubs

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
indexGetHeightEndpoint :: HeightRequest -> ServerM HeightResponse
indexGetHeightEndpoint (HeightRequest currency) = do
  mh <- runDb $ fmap (scannedHeightRecHeight . entityVal) <$> getScannedHeight currency
  pure $ HeightResponse $ fromMaybe 0 mh

indexGetBalanceEndpoint :: BalanceRequest -> ServerM BalanceResponse
indexGetBalanceEndpoint request = do
  maybeHistory <- getTxOutHistory $ balReqPubKeyScriptHash request
  let confirmedBalance = case maybeHistory of
        Just history -> getSum $ foldMap (Sum . txoValue) history
        Nothing      -> 0
  pure $ BalanceResponse { balRespConfirmed = confirmedBalance, balRespUnconfirmed = 0 }
  where
    txoValue (UTXO txo) = txOutCacheRecValue txo
    txoValue _ = 0

indexGetTxHashHistoryEndpoint :: TxHashHistoryRequest -> ServerM TxHashHistoryResponse
indexGetTxHashHistoryEndpoint request = do
  maybeHistory <- getTxOutHistory $ historyReqPubKeyScriptHash request
  case maybeHistory of
    Just history -> do
        let uniqueHistoryTxIds = nub . mconcat $ utxoHistoryTxIds <$> history
        txs <- getManyParsedExact $ cachedTxKey <$> uniqueHistoryTxIds
        let sortedTxs = sortOn txSorting txs
            historyItems = (\tx -> TxHashHistoryItem (txCacheRecHash tx) (txCacheRecBlockHeight tx)) <$> sortedTxs
        pure historyItems
    _-> pure []
  where
    utxoHistoryTxIds (UTXO txo)         = [txOutCacheRecTxHash txo]
    utxoHistoryTxIds (STXO (txo, stxo)) = [txOutCacheRecTxHash txo , txInCacheRecTxHash stxo]
    txSorting tx = (txCacheRecBlockHeight tx, txCacheRecBlockIndex  tx)

getBlockMetaSlice :: Currency -> BlockHeight -> Word64 -> ServerM [BlockMetaCacheRec]
getBlockMetaSlice currency startHeight amount = do
  let start = cachedMetaKey (currency, startHeight) 
      end   = BlockMetaCacheRecKey currency (pred $ startHeight + amount)
  slice <- safeEntrySlice start end
  let metaSlice = snd <$> slice
  pure metaSlice

indexGetBlockHeadersEndpoint :: BlockHeadersRequest -> ServerM BlockHeadersResponse
indexGetBlockHeadersEndpoint request = do
    slice <- getBlockMetaSlice (headersReqCurrency request) (headersReqStartIndex request) (headersReqAmount request)
    let blockHeaders = blockMetaCacheRecHeaderHexView <$> slice
    pure blockHeaders

indexGetBlockFiltersEndpoint :: BlockFiltersRequest -> ServerM BlockFiltersResponse
indexGetBlockFiltersEndpoint request = do
    slice <- getBlockMetaSlice (filtersReqCurrency request) (filtersReqStartIndex request) (filtersReqAmount request)
    let blockFilters = blockMetaCacheRecAddressFilterHexView <$> slice
    pure blockFilters

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
txBroadcastRequestEndpoint TxBroadcastRequest { txBroadcastReqCurrency = BTC }  = pure btcBroadcastResponse
txBroadcastRequestEndpoint TxBroadcastRequest { txBroadcastReqCurrency = ERGO } = pure ergoBroadcastResponse
