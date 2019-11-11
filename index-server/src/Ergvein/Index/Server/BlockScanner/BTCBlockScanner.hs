module Ergvein.Index.Server.BlockScanner.BTCBlockScanner where

import           Network.Haskoin.Util
import           Control.Monad
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Either
import           Data.List.Index
import           Data.Maybe
import           Database.Persist.Sql
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client
           
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.DB.Monad
import           Ergvein.Index.Server.DB.Queries
import           Ergvein.Index.Server.DB.Schema
import           Ergvein.Index.Server.Environment
import           Ergvein.Types.Currency
import           Ergvein.Types.Transaction
import           Ergvein.Index.Server.BlockScanner.Types

import           Data.Serialize                     as S
import qualified Data.ByteString                    as B
import qualified Data.HexString                     as HS
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Transaction        as HK

import qualified Data.Text.IO as T
import Data.Text (Text, pack)


txInfo :: HK.Tx -> TxHash -> ([TxInInfo], [TxOutInfo])
txInfo tx txHash = let
  withoutCoinbaseTx = filter $ (/= HK.nullOutPoint) . HK.prevOutput
  in (map txInInfo $ withoutCoinbaseTx $ HK.txIn tx, imap txOutInfo $ HK.txOut tx)
  where
    txInInfo txIn = let
      prevOutput = HK.prevOutput txIn
      in TxInInfo { txIn'txHash     = txHash
                  , txIn'txOutHash  = HK.txHashToHex $ HK.outPointHash prevOutput
                  , txIn'txOutIndex = fromIntegral $ HK.outPointIndex prevOutput
                  }
    txOutInfo txOutIndex txOut = let
      scriptOutputHash = encodeHex . B.reverse . S.encode . HK.doubleSHA256
      in TxOutInfo { txOut'txHash           = txHash
                   , txOut'pubKeyScriptHash = scriptOutputHash $ HK.scriptOutput txOut
                   , txOut'index            = fromIntegral txOutIndex
                   , txOut'value            = HK.outValue txOut
                   }    

blockTxInfos :: HK.Block -> BlockHeight -> ([TxInfo], [TxInInfo], [TxOutInfo])
blockTxInfos block txBlockHeight = mconcat $ txoInfosFromTx `imap` HK.blockTxns block
  where
    txoInfosFromTx txBlockIndex tx = let
      txHash = HK.txHashToHex $ HK.txHash tx
      txI = TxInfo { tx'hash = txHash
                   , tx'blockHeight = txBlockHeight
                   , tx'blockIndex  = fromIntegral txBlockIndex
                   }
      (txInI,txOutI) = txInfo tx txHash
      in ([txI], txInI, txOutI)


actualBTCHeight :: Config -> IO BlockHeight
actualBTCHeight cfg = fromIntegral <$> btcNodeClient cfg getBlockCount

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO ()
bTCBlockScanner env blockHeightToScan = do 
  blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
  maybeRawBlock <- btcNodeClient cfg $ flip getBlockRaw blockHash
  let rawBlock = fromMaybe blockParsingError maybeRawBlock 
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock
      (txInfos ,txInInfos, txOutInfos) = blockTxInfos parsedBlock blockHeightToScan
  --T.putStrLn $ pack $ show txInInfos
  runDbQuery dbPool $ insertTxs txInfos
  runDbQuery dbPool $ insertTxOuts txOutInfos
  runDbQuery dbPool $ insertTxIns txInInfos
  runDbQuery dbPool $ upsertScannedHeight BTC blockHeightToScan
  pure ()
  where
    cfg    = envConfig env
    dbPool = envPool env
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan