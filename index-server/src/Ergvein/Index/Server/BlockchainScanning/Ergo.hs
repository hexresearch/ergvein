module Ergvein.Index.Server.BlockchainScanning.Ergo where

import Control.Monad.Reader

import Ergvein.Types.Transaction
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.Environment
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Interfaces.Ergo.It.Api.NodeApi
import Network.Ergo.Api.Info
import Network.Ergo.Api.Blocks
import Ergvein.Interfaces.Ergo.Api
import Data.Maybe
import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Types.Currency
import Data.ByteString (ByteString)
import Data.Serialize
import Ergvein.Crypto.SHA256 
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as BS16

{-txInfo :: ErgoTransaction -> TxHash -> ([TxInInfo], [TxOutInfo])
txInfo tx txHash = let
  --withoutCoinbaseTx = filter $ (const True) . HK.prevOutput
  in (map txInInfo $ outputs tx, map txOutInfo $ inputs tx)
  where
    txInInfo txIn = let
      prevOutput = HK.prevOutput txIn
      in TxInInfo { txIn'txHash     = txHash
                  , txIn'txOutHash  = HK.txHashToHex $ HK.outPointHash prevOutput
                  , txIn'txOutIndex = fromIntegral $ HK.outPointIndex prevOutput
                  }
    txOutInfo txOut = let
      scriptOutputHash = encodeSHA256Hex . doubleSHA256
      in TxOutInfo { txOut'txHash           = txHash
                   , txOut'pubKeyScriptHash = scriptOutputHash $ HK.scriptOutput txOut
                   , txOut'index            = fromIntegral index
                   , txOut'value            = value txOut
                   }-}

txInfo :: ErgoTransaction -> TxHash -> ([TxInInfo], [TxOutInfo])
txInfo tx txHash = undefined

actualHeight :: ServerEnv -> IO BlockHeight
actualHeight env = do
  info <- flip runReaderT (env'ergoNodeClient env) $ getInfo
  pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight $ info

toHex :: ByteString -> T.Text
toHex = TE.decodeUtf8 . BS16.encode

blockTxInfos :: FullBlock -> BlockHeight -> BlockInfo
blockTxInfos block txBlockHeight = let
  (txInfos ,txInInfos, txOutInfos) = mconcat $  txoInfosFromTx <$> (transactions $ blockTransactions block)
  blockContent = BlockContentInfo txInfos txInInfos txOutInfos
  blockMeta = BlockMetaInfo ERGO txBlockHeight blockHeaderHexView
  in undefined
  where
    blockHeaderHexView = toHex $ undefined
    txoInfosFromTx tx = let
      txHash = T.pack $ show $  transactionId (tx :: ErgoTransaction)
      txI = TxInfo { tx'hash = txHash
                   , tx'blockHeight = txBlockHeight
                   , tx'blockIndex  = fromIntegral 4
                   }
      (txInI,txOutI) = txInfo tx txHash
      in ([txI], txInI, txOutI)

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = do
  headersAtHeight <- flip runReaderT (env'ergoNodeClient env) $ getHeaderIdsAtHeight $ Height $ fromIntegral blockHeightToScan
  let mainHeaderId = head headersAtHeight
  block <- flip runReaderT (env'ergoNodeClient env) $ getById mainHeaderId
  pure undefined
