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
import Data.List.Index
import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Types.Currency
import Data.ByteString (ByteString)
import Data.Serialize
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as BS16

txInfo :: ErgoTransaction -> TxHash -> ([TxInInfo], [TxOutInfo])
txInfo tx txHash = ([],[])

actualHeight :: ServerEnv -> IO BlockHeight
actualHeight env = do
  info <- flip runReaderT (env'ergoNodeClient env) $ getInfo
  pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight $ info

toHex :: ByteString -> T.Text
toHex = TE.decodeUtf8 . BS16.encode

blockTxInfos :: BlockTransactions -> BlockHeight -> BlockInfo
blockTxInfos blockTransactions txBlockHeight = let 
  (txInfos ,txInInfos, txOutInfos) = mconcat $ txoInfosFromTx `imap` transactions blockTransactions
  blockContent = BlockContentInfo txInfos txInInfos txOutInfos
  blockMeta = BlockMetaInfo ERGO txBlockHeight blockHeaderHexView
  in undefined
  where
    blockHeaderHexView = toHex $ encode $ undefined
    txoInfosFromTx txBlockIndex tx = let
      txHash = T.pack $ show $ tid tx
      txI = TxInfo { tx'hash = txHash
                   , tx'blockHeight = txBlockHeight
                   , tx'blockIndex  = fromIntegral txBlockIndex
                   }
      (txInI,txOutI) = txInfo tx txHash
      in ([txI], txInI, txOutI)

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = do
  headersAtHeight <- flip runReaderT (env'ergoNodeClient env) $ getHeaderIdsAtHeight $ Height $ fromIntegral blockHeightToScan
  let mainHeaderId = head headersAtHeight
  blockHeader <- flip runReaderT (env'ergoNodeClient env) $ getHeaderById mainHeaderId
  blockTransactions <- flip runReaderT (env'ergoNodeClient env) $ getTransactionsById mainHeaderId
  pure undefined