module Ergvein.Index.Server.BlockScanner.BTCBlockScanner where

import           Data.Either
import           Data.List.Index
import           Data.Maybe
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client
import           Network.Haskoin.Util

import           Ergvein.Index.Server.BlockScanner.Types
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.Environment
import           Ergvein.Types.Transaction
import           Ergvein.Types.Currency

import           Data.Serialize                     as S
import qualified Data.ByteString                    as B
import qualified Data.HexString                     as HS
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK

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

blockTxInfos :: HK.Block -> BlockHeight -> BlockInfo
blockTxInfos block txBlockHeight = let
  (txInfos ,txInInfos, txOutInfos) = mconcat $ txoInfosFromTx `imap` HK.blockTxns block
  blockContent = BlockContentInfo txInfos txInInfos txOutInfos
  blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHexView
  in BlockInfo blockMeta blockContent
  where
    blockHeaderHexView = HK.encodeHex $ S.encode $ HK.blockHeader block
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

bTCBlockScanner :: ServerEnv -> BlockHeight -> IO BlockInfo
bTCBlockScanner env blockHeightToScan = do 
  blockHash <- btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
  maybeRawBlock <- btcNodeClient cfg $ flip getBlockRaw blockHash
  let rawBlock = fromMaybe blockParsingError maybeRawBlock
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock
  pure $ blockTxInfos parsedBlock blockHeightToScan
  where
    cfg    = envConfig env
    dbPool = envPool env
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan