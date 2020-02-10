module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Either
import           Data.List.Index
import           Data.Maybe
import           Data.Serialize

import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client

import           Ergvein.Crypto.SHA256
import           Ergvein.Filters.Btc
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Cache.Monad
import           Ergvein.Index.Server.Cache.Queries
import           Ergvein.Index.Server.Cache.Schema
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.Environment
import           Ergvein.Index.Server.Utils
import           Ergvein.Types.Currency
import           Ergvein.Types.Transaction

import qualified Data.ByteString                    as B
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Constants          as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK

instance MonadLDB (ReaderT ServerEnv IO) where
  getDb = asks envLevelDBContext
  {-# INLINE getDb #-}

txInfo :: HK.Tx -> TxHash -> ([TxInInfo], [TxOutInfo])
txInfo tx txHash = let
  withoutCoinbaseTx = filter $ (/= HK.nullOutPoint) . HK.prevOutput
  in (map txInInfo $ withoutCoinbaseTx $ HK.txIn tx, imap txOutInfo $ HK.txOut tx)
  where
    txInInfo txIn = let
      prevOutput = HK.prevOutput txIn
      in TxInInfo { txInTxHash     = txHash
                  , txInTxOutHash  = HK.txHashToHex $ HK.outPointHash prevOutput
                  , txInTxOutIndex = fromIntegral $ HK.outPointIndex prevOutput
                  }
    txOutInfo txOutIndex txOut = let
      scriptOutputHash = encodeSHA256Hex . doubleSHA256
      in TxOutInfo { txOutTxHash           = txHash
                   , txOutPubKeyScriptHash = scriptOutputHash $ HK.scriptOutput txOut
                   , txOutIndex            = fromIntegral txOutIndex
                   , txOutValue            = HK.outValue txOut
                   }

blockTxInfos :: MonadLDB m => HK.Block -> BlockHeight -> HK.Network -> m BlockInfo
blockTxInfos block txBlockHeight nodeNetwork = do 
  let (txInfos ,txInInfos, txOutInfos) = mconcat $ txoInfosFromTx `imap` HK.blockTxns block
      blockContent = BlockContentInfo txInfos txInInfos txOutInfos
      txInfosMap = mapBy txHash $ HK.blockTxns block

  blockTxInSources <- mapM (txInSource txInfosMap) txInInfos
  let blockAddressFilter = HK.encodeHex $ encodeBtcAddrFilter $ makeBtcFilter nodeNetwork blockTxInSources block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHexView blockAddressFilter

  pure $ BlockInfo blockMeta blockContent 
  where
    txHash = HK.txHashToHex . HK.txHash
    blockHeaderHexView = HK.encodeHex $ encode $ HK.blockHeader block
    txInSource :: MonadLDB m => Map.Map TxId HK.Tx -> TxInInfo -> m HK.Tx
    txInSource blockTxMap txInput = 
      case Map.lookup txInId blockTxMap of
        Just    sourceTx -> pure sourceTx
        Nothing          -> fromChache
      where
        txInId = txInTxOutHash txInput
        decodeError = "error decoding btc txIn source transaction " <> show txInId
        fromChache = do
          src <- getParsedExact $ cachedTxKey txInId
          pure $ fromRight (error decodeError) $ decode $ fromJust $ HK.decodeHex $ txCacheRecHexView src
    txoInfosFromTx txBlockIndex tx = let
      txHash' = txHash tx
      txI = TxInfo { txHash = txHash'
                   , txHexView = HK.encodeHex $ encode tx 
                   , txBlockHeight = txBlockHeight
                   , txBlockIndex  = fromIntegral txBlockIndex
                   }
      (txInI,txOutI) = txInfo tx txHash'
      in ([txI], txInI, txOutI)

actualHeight :: Config -> IO BlockHeight
actualHeight cfg = fromIntegral <$> btcNodeClient cfg getBlockCount

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = flip runReaderT env $ do
  blockHash <- liftIO $ btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
  maybeRawBlock <- liftIO $ btcNodeClient cfg $ flip getBlockRaw blockHash
  let rawBlock = fromMaybe blockParsingError maybeRawBlock
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock
      currentNetwork = envBitconNodeNetwork env
  
  blockTxInfos parsedBlock blockHeightToScan currentNetwork
  where
    cfg    = envServerConfig env
    dbPool = envPersistencePool env
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan