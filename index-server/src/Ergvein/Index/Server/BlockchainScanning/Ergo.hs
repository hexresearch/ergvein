module Ergvein.Index.Server.BlockchainScanning.Ergo where

import  Control.Monad.Reader
import  Data.List.Index
import  Data.Maybe
import  Data.Serialize

import Ergvein.Crypto.SHA256
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.Environment
import Ergvein.Interfaces.Ergo.Api
import Ergvein.Interfaces.Ergo.It.Api.NodeApi
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Text

import           Network.Ergo.Api.Blocks
import           Network.Ergo.Api.Client
import           Network.Ergo.Api.Info
import qualified Network.Ergo.Api.Utxo    as UtxoApi


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
      scriptOutputHash = encodeSHA256Hex . doubleSHA256
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

actualHeight :: ServerEnv -> IO BlockHeight
actualHeight env = do
    info <- runReaderT getInfo (envErgoNodeClient env)
    pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight info

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = flip runReaderT (envErgoNodeClient env) $ do
  headersAtHeight <- getHeaderIdsAtHeight
      $ Height
      $ fromIntegral blockHeightToScan

  let mainChainId = head headersAtHeight

  block <- getById mainChainId
  blockInfo <- blockTxInfos block blockHeightToScan
  pure blockInfo