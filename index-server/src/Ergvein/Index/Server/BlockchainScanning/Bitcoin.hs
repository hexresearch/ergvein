module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Monad.Reader
import           Control.Monad.Logger
import           Data.Either
import           Data.List.Index
import           Data.Maybe
import  qualified         Data.Map.Strict as Map
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client

import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.Environment
import           Ergvein.Types.Transaction
import           Ergvein.Types.Currency
import           Ergvein.Crypto.SHA256
import           Ergvein.Filters.Btc
import           Ergvein.Index.Server.Cache.Monad
import           Ergvein.Index.Server.Cache.Schema
import           Ergvein.Index.Server.Cache.Queries
import           Ergvein.Index.Server.Utils

import Data.Serialize
import qualified Data.ByteString                    as B
import qualified Data.HexString                     as HS
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK
import qualified Network.Haskoin.Constants          as HK

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

blockTxInfos :: MonadLDB m => HK.Block -> BlockHeight -> m BlockInfo
blockTxInfos block txBlockHeight = do 
  let (txInfos ,txInInfos, txOutInfos) = mconcat $ txoInfosFromTx `imap` HK.blockTxns block
      blockContent = BlockContentInfo txInfos txInInfos txOutInfos
      txInfosMap = mapBy txHash $ HK.blockTxns block
      txInSource :: MonadLDB m => TxInInfo -> m HK.Tx
      txInSource input = do fromMaybe defa (pure <$> (Map.lookup txId txInfosMap))
        where
          txId = txInTxOutHash input
          defa = do
            x <- getParsedExact $ cachedTxKey txId
            pure . fromRight (error "") . decode . fromJust . HK.decodeHex . txCacheRecHexView $ x

  txsEnts <- sequence $ txInSource <$> txInInfos
  let blockAddressFilter = HK.encodeHex $ encodeBtcAddrFilter $ makeBtcFilter HK.btcTest txsEnts block
  let blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHexView blockAddressFilter
  pure $ BlockInfo blockMeta blockContent 
  where
    txHash = HK.txHashToHex . HK.txHash
    blockHeaderHexView = HK.encodeHex $ encode $ HK.blockHeader block
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

instance MonadLDB (ReaderT ServerEnv IO) where
  getDb = asks envLevelDBContext
  {-# INLINE getDb #-}

blockInfo :: ServerEnv -> BlockHeight -> IO BlockInfo
blockInfo env blockHeightToScan = flip runReaderT env $ do
  blockHash <- liftIO $ btcNodeClient cfg $ flip getBlockHash $ fromIntegral blockHeightToScan
  maybeRawBlock <- liftIO $ btcNodeClient cfg $ flip getBlockRaw blockHash
  let rawBlock = fromMaybe blockParsingError maybeRawBlock
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock
  
  blockTxInfos parsedBlock blockHeightToScan
  where
    cfg    = envServerConfig env
    dbPool = envPersistencePool env
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan
