module Ergvein.Index.Server.BlockchainScanning.Ergo where

import  Control.Monad.Reader
import  Data.List.Index
import  Data.Maybe
import  Data.Serialize

import Ergvein.Crypto.Hash
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Environment
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
import Control.Monad.IO.Unlift

txInfo :: ApiMonad m => ErgoTransaction -> TxHash -> m ([TxInInfo], [TxOutInfo])
txInfo tx txHash = do
  let txOuts = txOutInfo <$> outputs tx 
  txIns <- forM (dataInputs tx) txInInfo
  pure $ (txIns, txOuts)
  where
    txInInfo :: ApiMonad m => ErgoTransactionDataInput -> m TxInInfo
    txInInfo txIn = do
      box <- UtxoApi.getById $ boxId (txIn :: ErgoTransactionDataInput)
      pure $ TxInInfo { txInTxHash     = txHash
                      , txInTxOutHash  = bs2Hex $ unTransactionId $ fromJust $ transactionId (box :: ErgoTransactionOutput)
                      , txInTxOutIndex = fromIntegral $ fromJust $ index box
                      }

    txOutInfo txOut = let
      scriptOutputHash = showt . doubleSHA256
      in TxOutInfo { txOutTxHash           = txHash
                   , txOutPubKeyScriptHash = scriptOutputHash $ unErgoTree $ ergoTree txOut
                   , txOutIndex            = fromIntegral $ fromJust $ index txOut
                   , txOutValue            = value txOut
                   }

blockTxInfos :: ApiMonad m => FullBlock -> BlockHeight -> m BlockInfo
blockTxInfos block txBlockHeight = do
  (txInfos ,txInInfos, txOutInfos) <- mconcat <$> (sequence $ txoInfosFromTx `imap` (transactions $ blockTransactions block))
  let blockContent = BlockContentInfo txInfos txInInfos txOutInfos
      blockAddressFilter = const "ergoBlockAddressFilter" $ undefined
      blockMeta = BlockMetaInfo ERGO (fromIntegral txBlockHeight) blockHeaderHexView blockAddressFilter
  pure $ BlockInfo blockMeta blockContent
  where
    blockHeaderHexView = bs2Hex $ encode $ headerFromApi $ header block
    txoInfosFromTx :: ApiMonad m => Int -> ErgoTransaction -> m ([TxInfo], [TxInInfo], [TxOutInfo])
    txoInfosFromTx txBlockIndex tx = do
      let txHash = bs2Hex $ unTransactionId $ transactionId (tx :: ErgoTransaction)
          txI = TxInfo { txHash = txHash
                       , txHexView = "txHexView"
                       , txBlockHeight = txBlockHeight
                       , txBlockIndex  = fromIntegral txBlockIndex
                       }
      (txInI,txOutI) <- txInfo tx txHash
      pure ([txI], txInI, txOutI)

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