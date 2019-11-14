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
import Control.Monad.IO.Unlift
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Monad
import Database.Persist.Types
import Ergvein.Index.Server.DB.Schema
import Database.Persist.Sql
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Ergvein.Index.Server.BlockchainCache 
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Ergvein.Index.Server.BlockScanner.Types
import Database.LevelDB
import Control.Monad.Trans.Resource (release)

indexServer :: IndexApi AsServerM
indexServer = IndexApi
    { indexGetBalance = indexGetBalanceEndpoint
    , indexGetTxHashHistory = indexGetTxHashHistoryEndpoint
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
  b <- runResourceT $ open "/tmp/leveltest" defaultOptions { createIfMissing = True, cacheSize = 2048 }
  cacheTVar <- getCache
  cache <- liftIO $ atomically $ readTVar cacheTVar
  pure btcBalance {balRespConfirmed = cache'byScript cache Map.! balReqPubKeyScriptHash req }

indexGetBalanceEndpoint BalanceRequest { balReqCurrency = ERGO } = pure ergoBalance

indexGetTxHashHistoryEndpoint :: TxHashHistoryRequest -> ServerM TxHashHistoryResponse
indexGetTxHashHistoryEndpoint  req@(TxHashHistoryRequest{ historyReqCurrency = BTC }) = do
    txInEntities <- runDb getAllTxIn
    txOutEntities <- runDb getAllTxOut
    txEntities <- runDb getAllTx
    let txs = entityVal <$> txEntities
        txOuts = Map.fromList $ (\e-> ((txOutRecTxHash e, txOutRecPubKeyScriptHash e), e)) <$> entityVal <$> txOutEntities
        txOuts' = Map.fromList $ (\e-> ((txOutRecTxHash e, txOutRecIndex e), txOutRecPubKeyScriptHash e)) <$> entityVal <$> txOutEntities
        f x = (txInRecTxHash x,  [txOuts' Map.! (txInRecTxOutHash x, txInRecTxOutIndex x)])
        txIns = Map.fromListWith (++) $ f <$> entityVal <$> txInEntities
        p1 tx = Map.member (txRecHash tx, historyReqPubKeyScriptHash req) txOuts
        p2 tx = any (elem (historyReqPubKeyScriptHash req)) $ txIns Map.!? (txRecHash tx)
        mp x = TxHashHistoryItem {historyItemTxHash = txRecHash x, historyItemBlockHeight = txRecBlockHeigh x }
        r = mp <$> filter (\tx -> p1 tx || p2 tx) txs
    pure r

indexGetTxHashHistoryEndpoint TxHashHistoryRequest { historyReqCurrency = ERGO } = pure ergoHistory

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
