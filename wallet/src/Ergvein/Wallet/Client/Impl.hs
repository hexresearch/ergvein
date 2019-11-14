module Ergvein.Wallet.Client.Impl
  (
    getBalanceImpl
  , getTxHashHistoryImpl
  , getTxMerkleProofImpl
  , getTxHexViewImpl
  , getTxFeeHistogramImpl
  , txBroadcastImpl
  , getBalanceImpl'
  , getTxHashHistoryImpl'
  , getTxMerkleProofImpl'
  , getTxHexViewImpl'
  , getTxFeeHistogramImpl'
  , txBroadcastImpl'
  ) where

import Ergvein.Index.API
import Ergvein.Index.API.Types
import Ergvein.Index.API.V1
import Ergvein.Text
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Client.Util
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util

getBalanceImpl :: forall t m . MonadFrontBase t m
  => Text -> Event t BalanceRequest -> m (Event t BalanceResponse)
getBalanceImpl url reqE = do
  logInfo $ ((<>) "Request getBalance " . showt) <$> reqE
  handleDangerMsg =<< getBalanceImpl' (pure url) reqE

getTxHashHistoryImpl :: forall t m . MonadFrontBase t m
  => Text -> Event t TxHashHistoryRequest -> m (Event t TxHashHistoryResponse)
getTxHashHistoryImpl url reqE = do
  logInfo $ ((<>) "Request getTxHashHistory " . showt) <$> reqE
  handleDangerMsg =<< getTxHashHistoryImpl' (pure url) reqE

getTxMerkleProofImpl :: forall t m . MonadFrontBase t m
  => Text -> Event t TxMerkleProofRequest -> m (Event t TxMerkleProofResponse)
getTxMerkleProofImpl url reqE = do
  logInfo $ ((<>) "Request getTxMerkleProof " . showt) <$> reqE
  handleDangerMsg =<< getTxMerkleProofImpl' (pure url) reqE

getTxHexViewImpl :: forall t m . MonadFrontBase t m
  => Text -> Event t TxHexViewRequest -> m (Event t TxHexViewResponse)
getTxHexViewImpl url reqE = do
  logInfo $ ((<>) "Request getTxHexView " . showt) <$> reqE
  handleDangerMsg =<< getTxHexViewImpl' (pure url) reqE

getTxFeeHistogramImpl :: forall t m . MonadFrontBase t m
  => Text -> Event t TxFeeHistogramRequest -> m (Event t TxFeeHistogramResponse)
getTxFeeHistogramImpl url reqE = do
  logInfo $ ((<>) "Request getTxFeeHistogram " . showt) <$> reqE
  handleDangerMsg =<< getTxFeeHistogramImpl' (pure url) reqE

txBroadcastImpl :: forall t m . MonadFrontBase t m
  => Text -> Event t TxBroadcastRequest -> m (Event t TxBroadcastResponse)
txBroadcastImpl url reqE = do
  logInfo $ ((<>) "Request txBroadcast " . showt) <$> reqE
  handleDangerMsg =<< txBroadcastImpl' (pure url) reqE

getBalanceImpl' :: forall t m . MonadBaseConstr t m
  => Dynamic t Text -> Event t BalanceRequest -> m (Event t (Either ClientError BalanceResponse))
getBalanceImpl' durl reqE = endpoint1' endpoint reqE
  where endpoint = indexGetBalance $ indexerV1_ (Proxy :: Proxy m) durl

getTxHashHistoryImpl' :: forall t m . MonadBaseConstr t m
  => Dynamic t Text -> Event t TxHashHistoryRequest -> m (Event t (Either ClientError TxHashHistoryResponse))
getTxHashHistoryImpl' durl reqE = endpoint1' endpoint reqE
  where endpoint = indexGetTxHashHistory $ indexerV1_ (Proxy :: Proxy m) durl

getTxMerkleProofImpl' :: forall t m . MonadBaseConstr t m
  => Dynamic t Text -> Event t TxMerkleProofRequest -> m (Event t (Either ClientError TxMerkleProofResponse))
getTxMerkleProofImpl' durl reqE = endpoint1' endpoint reqE
  where endpoint = indexGetTxMerkleProof $ indexerV1_ (Proxy :: Proxy m) durl

getTxHexViewImpl' :: forall t m . MonadBaseConstr t m
  => Dynamic t Text -> Event t TxHexViewRequest -> m (Event t (Either ClientError TxHexViewResponse))
getTxHexViewImpl' durl reqE = endpoint1' endpoint reqE
  where endpoint = indexGetTxHexView $ indexerV1_ (Proxy :: Proxy m) durl

getTxFeeHistogramImpl' :: forall t m . MonadBaseConstr t m
  => Dynamic t Text -> Event t TxFeeHistogramRequest -> m (Event t (Either ClientError TxFeeHistogramResponse))
getTxFeeHistogramImpl' durl reqE = endpoint1' endpoint reqE
  where endpoint = indexGetTxFeeHistogram $ indexerV1_ (Proxy :: Proxy m) durl

txBroadcastImpl' :: forall t m . MonadBaseConstr t m
  => Dynamic t Text -> Event t TxBroadcastRequest -> m (Event t (Either ClientError TxBroadcastResponse))
txBroadcastImpl' durl reqE = endpoint1' endpoint reqE
  where endpoint = indexTxBroadcast $ indexerV1_ (Proxy :: Proxy m) durl
