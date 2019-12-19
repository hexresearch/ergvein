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
import Network.Ergo.Api.Client
import qualified Network.Ergo.Api.Utxo as UtxoApi
import Ergvein.Crypto.SHA256 
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base16 as BS16
import           Data.List.Index


toHex :: ByteString -> T.Text
toHex = TE.decodeUtf8 . BS16.encode

actualHeight :: ServerEnv -> IO BlockHeight
actualHeight env = do
  info <- flip runReaderT (env'ergoNodeClient env) $ getInfo
  pure $ fromIntegral $ fromMaybe 0 $ bestBlockHeight $ info

txInInfos :: ApiMonad m => ErgoTransaction -> m [TxInInfo]
txInInfos tx = do
  let txHash = T.pack $ show $ unTransactionId $ transactionId (tx :: ErgoTransaction)
  let d =  (boxId :: ErgoTransactionDataInput -> TransactionBoxId) <$> dataInputs tx
  r <- forM d UtxoApi.getById 
  pure $ (\x -> TxInInfo txHash (toHex $ unTransactionId $ fromJust $ transactionId (x :: ErgoTransactionOutput)) (fromIntegral $ fromJust $ index (x :: ErgoTransactionOutput))) <$> r
  
txInfo' :: BlockHeight -> Int -> ErgoTransaction -> TxInfo
txInfo' txBlockHeight txBlockIndex tx = TxInfo (T.pack $ show $ unTransactionId $ transactionId (tx :: ErgoTransaction)) (txBlockHeight) (fromIntegral txBlockIndex)

txOutInfos ::  ErgoTransaction -> [TxOutInfo]
txOutInfos tx = let
  txHash = T.pack $ show $ unTransactionId $ transactionId (tx :: ErgoTransaction)
  f x = TxOutInfo txHash undefined (fromIntegral $ fromJust $ index $ x) (value x)
  in f <$> outputs tx

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = do
  headersAtHeight <- flip runReaderT (env'ergoNodeClient env) $ getHeaderIdsAtHeight $ Height $ fromIntegral blockHeightToScan
  let mainHeaderId = head headersAtHeight
  block <- flip runReaderT (env'ergoNodeClient env) $ getById mainHeaderId

  txins <- flip runReaderT (env'ergoNodeClient env) $ mconcat <$> forM (transactions $ blockTransactions block) txInInfos

  let txs = txInfo' blockHeightToScan `imap` (transactions $ blockTransactions block)
  let txouts = mconcat $ txOutInfos <$> (transactions $ blockTransactions block)

  let blockContent = BlockContentInfo txs txins txouts
  let blockMeta = BlockMetaInfo ERGO (fromIntegral blockHeightToScan) $ T.pack "blockHeaderHexView"

  pure $ BlockInfo blockMeta blockContent
