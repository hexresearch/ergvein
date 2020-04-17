module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Either
import           Data.List.Index
import           Data.Maybe
import           Data.Serialize

import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client

import           Ergvein.Crypto.Hash
import           Ergvein.Filters.Btc
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Cache.Monad
import           Ergvein.Index.Server.Cache.Queries
import           Ergvein.Index.Server.Cache.Schema
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.Environment
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Transaction

import qualified Data.ByteString                    as B
import qualified Data.HashSet                       as Set
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Constants          as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK
import qualified Data.Text                          as T

import Debug.Trace

import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad

blockTxInfos :: MonadLDB m => HK.Block -> BlockHeight -> HK.Network -> m BlockInfo
blockTxInfos block txBlockHeight nodeNetwork = do 
  let (txInfos ,txInInfos) = mconcat $ txInfo <$> HK.blockTxns block
      txInfosMap = mapBy (HK.txHashToHex . HK.txHash) $ HK.blockTxns block
      uniqueTxInIds = Set.toList $ Set.fromList txInInfos
  
  blockTxInSources <- mapM (txInSource txInfosMap) uniqueTxInIds

  let blockAddressFilter = HK.encodeHex $ encodeBtcAddrFilter $ makeBtcFilter nodeNetwork blockTxInSources block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHashHexView blockAddressFilter

  pure $ BlockInfo blockMeta txInInfos txInfos 
  where
    blockHeaderHashHexView = HK.blockHashToHex $ HK.headerHash $ HK.blockHeader block
    txInSource :: MonadLDB m => Map.Map TxId HK.Tx -> T.Text -> m HK.Tx
    txInSource blockTxMap txInId = 
      case Map.lookup txInId blockTxMap of
        Just    sourceTx -> pure sourceTx
        Nothing          -> fromChache
      where
        decodeError = "error decoding btc txIn source transaction " <> show txInId
        fromChache = do
          src <- getParsedExact $ cachedTxKey txInId
          pure $ fromRight (error decodeError) $ decode $ fromJust $ HK.decodeHex $ txCacheRecHexView src
    txInfo :: HK.Tx -> ([TxInfo2], [T.Text])
    txInfo tx = let
      withoutCoinbaseTx = filter $ (/= HK.nullOutPoint) 
      txI = TxInfo2 { txHash2 = HK.txHashToHex $ HK.txHash tx
                    , txHexView2 = HK.encodeHex $ encode tx 
                    , txOutputsCount = fromIntegral $ length $ HK.txOut tx
                    }
      txInI = HK.txHashToHex . HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
      in ([txI], txInI)

--actualHeight :: Config -> IO BlockHeight
--actualHeight cfg = fromIntegral <$> btcNodeClient cfg getBlockCount

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount

blockInfo :: (BitcoinApiMonad m,  HasBitcoinNodeNetwork m, MonadLDB m) => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan =  do
  blockHash <- nodeRpcCall $ flip getBlockHash $ fromIntegral blockHeightToScan
  maybeRawBlock <- nodeRpcCall $ flip getBlockRaw blockHash
  let rawBlock = fromMaybe blockParsingError maybeRawBlock
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock
  
  currentNetwork <- currentBitcoinNetwork 
  
  blockTxInfos parsedBlock blockHeightToScan currentNetwork
  where
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan