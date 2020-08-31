module Ergvein.Index.Server.BlockchainScanning.Ergo where

import  Control.Monad.Reader
import  Data.List.Index
import  Data.Maybe
import  Data.Serialize

import Ergvein.Crypto.Hash
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Interfaces.Ergo.Api
import Ergvein.Interfaces.Ergo.It.Api.NodeApi
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import           Network.Ergo.Api.Blocks
import           Network.Ergo.Api.Client
import           Network.Ergo.Api.Info
import qualified Network.Ergo.Api.Utxo    as UtxoApi
import qualified Data.ByteString.Short as BSS
import Control.Monad.IO.Unlift

txInfo :: ApiMonad m => ErgoTransaction -> m ([TxInfo], [TxHash])
txInfo tx = do
  let info = TxInfo { txHash =  TxHash $ BSS.toShort $ unTransactionId $ transactionId (tx :: ErgoTransaction)
                    , txBytes = mempty
                    , txOutputsCount = fromIntegral $ length $ dataInputs tx
                    }
  txIns <- forM (dataInputs tx) txInInfo
  let spentTxIds = TxHash . BSS.toShort . unTransactionId . fromJust . (transactionId :: ErgoTransactionOutput -> Maybe TransactionId) <$> txIns
  pure $ ([info], spentTxIds)
  where
    txInInfo txIn = UtxoApi.getById $ boxId (txIn :: ErgoTransactionDataInput)

blockTxInfos :: ApiMonad m => FullBlock -> BlockHeight -> m BlockInfo
blockTxInfos block txBlockHeight = do
  (txInfos , spentTxsIds) <- mconcat <$> mapM txInfo (transactions $ blockTransactions block)
  let blockHeaderHashHexView = mempty --TODO
      prevBlockHeaderHashHexView = mempty --TODO
      blockAddressFilter = mempty --TODO
      blockMeta = BlockMetaInfo ERGO (fromIntegral txBlockHeight) blockHeaderHashHexView prevBlockHeaderHashHexView blockAddressFilter
  pure $ BlockInfo blockMeta spentTxsIds txInfos

actualHeight :: ApiMonad m => m BlockHeight
actualHeight = do
    info <- getInfo
    pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight info

blockInfo :: ApiMonad m  => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan = do
  headersAtHeight <- getHeaderIdsAtHeight
      $ Height
      $ fromIntegral blockHeightToScan

  let mainChainId = head headersAtHeight

  block <- getById mainChainId
  blockInfo <- blockTxInfos block blockHeightToScan
  pure blockInfo
