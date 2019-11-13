module Ergvein.Wallet.Client.Impl
  (
    getBalanceImpl
  , getTxHashHistoryImpl
  , getTxMerkleProofImpl
  , getTxHexViewImpl
  , getTxFeeHistogramImpl
  , txBroadcastImpl
  ) where

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Text
import Ergvein.Wallet.Client.Util
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util

getBalanceImpl :: forall t m . MonadFrontBase t m
  => Event t BalanceRequest -> Text -> m (Event t BalanceResponse)
getBalanceImpl reqE url = do
  logInfo $ ((<>) "Request getBalance " . showt) <$> reqE
  let endpoint = indexGetBalance $ indexerV1_ (Proxy :: Proxy m) url
  endpoint1 endpoint reqE

getTxHashHistoryImpl :: forall t m . MonadFrontBase t m
  => Event t TxHashHistoryRequest -> Text -> m (Event t TxHashHistoryResponse)
getTxHashHistoryImpl reqE url = do
  logInfo $ ((<>) "Request getTxHashHistory " . showt) <$> reqE
  let endpoint = indexGetTxHashHistory $ indexerV1_ (Proxy :: Proxy m) url
  endpoint1 endpoint reqE

getTxMerkleProofImpl :: forall t m . MonadFrontBase t m
  => Event t TxMerkleProofRequest -> Text -> m (Event t TxMerkleProofResponse)
getTxMerkleProofImpl reqE url = do
  logInfo $ ((<>) "Request getTxMerkleProof " . showt) <$> reqE
  let endpoint = indexGetTxMerkleProof $ indexerV1_ (Proxy :: Proxy m) url
  endpoint1 endpoint reqE

getTxHexViewImpl :: forall t m . MonadFrontBase t m
  => Event t TxHexViewRequest -> Text -> m (Event t TxHexViewResponse)
getTxHexViewImpl reqE url = do
  logInfo $ ((<>) "Request getTxHexView " . showt) <$> reqE
  let endpoint = indexGetTxHexView $ indexerV1_ (Proxy :: Proxy m) url
  endpoint1 endpoint reqE

getTxFeeHistogramImpl :: forall t m . MonadFrontBase t m
  => Event t TxFeeHistogramRequest -> Text -> m (Event t TxFeeHistogramResponse)
getTxFeeHistogramImpl reqE url = do
  logInfo $ ((<>) "Request getTxFeeHistogram " . showt) <$> reqE
  let endpoint = indexGetTxFeeHistogram $ indexerV1_ (Proxy :: Proxy m) url
  endpoint1 endpoint reqE

txBroadcastImpl :: forall t m . MonadFrontBase t m
  => Event t TxBroadcastRequest -> Text -> m (Event t TxBroadcastResponse)
txBroadcastImpl reqE url = do
  logInfo $ ((<>) "Request txBroadcast " . showt) <$> reqE
  let endpoint = indexTxBroadcast $ indexerV1_ (Proxy :: Proxy m) url
  endpoint1 endpoint reqE
