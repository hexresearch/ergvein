module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Lens.Combinators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Either
import           Data.List.Index
import           Data.Maybe
import           Data.Serialize
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client

import           Ergvein.Crypto.Hash
import           Ergvein.Filters.Btc.Mutable
import           Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Cache.Monad
import           Ergvein.Index.Server.Cache.Queries
import           Ergvein.Index.Server.Cache.Schema
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Transaction
import           Ergvein.Index.Server.Dependencies

import qualified Data.Set                           as Set
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Constants          as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK


blockTxInfos :: MonadLDB m => HK.Block -> BlockHeight -> HK.Network -> m BlockInfo
blockTxInfos block txBlockHeight nodeNetwork = do
  let (txInfos , spentTxsIds) = mconcat $ txInfo <$> HK.blockTxns block

  uniqueSpentTxs <- mapM spentTxSource $ uniqueElements spentTxsIds

  let blockHeaderHashHexView = HK.blockHashToHex $ HK.headerHash $ HK.blockHeader block
  blockAddressFilter <- fmap HK.encodeHex $ encodeBtcAddrFilter =<< makeBtcFilter nodeNetwork uniqueSpentTxs block
  let blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHashHexView blockAddressFilter

  pure $ BlockInfo blockMeta spentTxsIds txInfos
  where
    blockTxMap = mapBy (HK.txHashToHex . HK.txHash) $ HK.blockTxns block
    spentTxSource :: MonadLDB m => TxHash -> m HK.Tx
    spentTxSource txInId =
      case Map.lookup txInId blockTxMap of
        Just    sourceTx -> pure sourceTx
        Nothing          -> fromChache
      where
        decodeError = "error decoding btc txIn source transaction " <> show txInId
        fromChache = do
          src <- getParsedExact $ cachedTxKey txInId
          pure $ fromRight (error decodeError) $ decode $ fromJust $ HK.decodeHex $ txCacheRecHexView src

    txInfo :: HK.Tx -> ([TxInfo], [TxHash])
    txInfo tx = let
      withoutDataCarrier = none HK.isDataCarrier . HK.decodeOutputBS . HK.scriptOutput
      info = TxInfo { txHash = HK.txHashToHex $ HK.txHash tx
                    , txHexView = HK.encodeHex $ encode tx
                    , txOutputsCount = fromIntegral $ length $ filter withoutDataCarrier $  HK.txOut tx
                    }
      withoutCoinbaseTx = filter $ (/= HK.nullOutPoint)
      spentTxInfo = HK.txHashToHex . HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
      in ([info], spentTxInfo)

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount

blockInfo :: (BitcoinApiMonad m,  HasBitcoinNodeNetwork m, MonadLDB m) => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan =  do
  blockHash <- nodeRpcCall $ (`getBlockHash` fromIntegral blockHeightToScan)
  maybeRawBlock <- nodeRpcCall $ (`getBlockRaw` blockHash)

  let rawBlock = fromMaybe blockParsingError maybeRawBlock
      parsedBlock = fromRight blockGettingError $ decode $ HS.toBytes rawBlock

  currentNetwork <- currentBitcoinNetwork

  blockTxInfos parsedBlock blockHeightToScan currentNetwork
  where
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightToScan
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightToScan
