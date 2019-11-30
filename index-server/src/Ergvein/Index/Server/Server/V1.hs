module Ergvein.Index.Server.Server.V1 where

import Data.Proxy
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Index.Server.Monad
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Server.DB.Monad
import Data.Maybe
import Data.List
import Ergvein.Index.Server.BlockchainCache 
import Database.LevelDB.Iterator 
import qualified Database.LevelDB.Streaming as LDB
import Data.Default
import Data.Flat
import Data.Either
import Control.Monad.IO.Class
import qualified Data.Text.IO as T
import Data.Text (Text, pack)
import qualified Network.Haskoin.Util as HK
import qualified Data.ByteString as BS

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
  db <- getDb
  maybeUTXOs <- (fmap $ fmap $ unflatExact @[CachedTxOut]) $ getInKeySpace db cachedTxOutKey $ balReqPubKeyScriptHash req
  let utxos = fromMaybe [] maybeUTXOs
  tutxos <- mapM (f db) utxos
  pure btcBalance { balRespConfirmed = foldl (+) 0 tutxos }
  where
    f db x = do
        stxo <- getInKeySpace db cachedTxInKey $ (cachedTxOut'txHash x, cachedTxOut'index  x)
        pure $ case stxo of
            Just s -> 0
            Nothing -> cachedTxOut'value x

indexGetBalanceEndpoint BalanceRequest { balReqCurrency = ERGO } = pure ergoBalance

indexGetTxHashHistoryEndpoint :: TxHashHistoryRequest -> ServerM TxHashHistoryResponse
indexGetTxHashHistoryEndpoint  req@(TxHashHistoryRequest{ historyReqCurrency = BTC }) = do
  db <- getDb
  maybeUTXOs <- (fmap $ fmap $ unflatExact @[CachedTxOut]) (getInKeySpace db cachedTxOutKey $ historyReqPubKeyScriptHash req)
  let utxos = fromMaybe [] maybeUTXOs
  tutxos <- mapM (f db) utxos
  txs <- mapM (fmap (unflatExact @CachedTx) . fmap fromJust . getInKeySpace db cachedTxKey) $ nub $ mconcat tutxos
  pure $ (\x -> TxHashHistoryItem (cachedTx'hash x) (cachedTx'blockHeight x) ) <$> sortOn (\x-> (cachedTx'blockHeight x, cachedTx'blockIndex  x)) txs
  where
      f db x = do
          stxo <- (fmap $ fmap $ unflatExact @TxHash) $  getInKeySpace db cachedTxInKey (cachedTxOut'txHash x, cachedTxOut'index x)
          pure $ case stxo of
              Just s -> [cachedTxOut'txHash x , s]
              Nothing -> [cachedTxOut'txHash x]

indexGetTxHashHistoryEndpoint TxHashHistoryRequest { historyReqCurrency = ERGO } = pure ergoHistory

indexGetBlockHeadersEndpoint :: BlockHeadersRequest -> ServerM BlockHeadersResponse
indexGetBlockHeadersEndpoint request = do
    db <- getDb
    i <- createIter db  def
    let range = LDB.KeyRange start $ c
    slice <- LDB.toList $ LDB.entrySlice i range LDB.Asc
    pure $ fromRight (error "key") . unflat @Text . snd <$> slice
    where
        start = flat (headersReqCurrency request, headersReqStartIndex request)
        end = (headersReqCurrency request, headersReqStartIndex request + headersReqAmount request)
        c x = compare (fromRight (error "key") $ unflat x) end 

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