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

txoInfosFromTx :: HK.Tx -> ([SpentTXOInfo], [TXOInfo])
txoInfosFromTx tx = let
  txoInfo txOutIndex txOut = let
    scriptOutputHash = encodeHex . B.reverse . S.encode . HK.doubleSHA256
    in TXOInfo {  txo'txHash     = txHash
                , txo'scriptHash = scriptOutputHash $ HK.scriptOutput txOut
                , txo'outIndex   = fromIntegral txOutIndex
                , txo'outValue   = HK.outValue txOut
                }
  txHash = HK.txHashToHex $ HK.txHash tx
  spentTXOInfo txIn = let
    spentOut = HK.prevOutput txIn
    in SpentTXOInfo { stxo'txHash   = txHash
                    , stxo'txoHash  = HK.txHashToHex $ HK.outPointHash spentOut
                    , stxo'outIndex = fromIntegral $ HK.outPointIndex spentOut
                    }
  withoutCoinbaseTx = filter $ (/= HK.nullOutPoint) . HK.prevOutput
  in (map spentTXOInfo $ withoutCoinbaseTx $ HK.txIn tx, imap txoInfo $ HK.txOut tx)

blockTXOInfos :: HK.Block -> ([SpentTXOInfo], [TXOInfo])
blockTXOInfos block = mconcat $ txoInfosFromTx <$> HK.blockTxns block

actualBTCHeight :: Config -> IO BlockHeight
actualBTCHeight cfg = fromIntegral <$> btcNodeClient cfg getBlockCount

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO ()
bTCBlockScanner env blockHeightToScan = do 
  blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
  maybeRawBlock <- btcNodeClient cfg $ flip getBlockRaw blockHash
  let rawBlock = fromMaybe blockParsingError maybeRawBlock 
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock
      (stxos, txos) = blockTXOInfos parsedBlock
  runDbQuery dbPool $ insertTXOs txos
  forM_ stxos (\info -> runDbQuery dbPool $ insertSTXO info)
  runDbQuery dbPool $ upsertScannedHeight BTC blockHeightToScan
  pure ()
  where
    cfg    = envConfig env
    dbPool = envPool env
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan