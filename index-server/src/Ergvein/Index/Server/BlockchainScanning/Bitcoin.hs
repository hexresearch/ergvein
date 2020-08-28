module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Concurrent
import           Control.Lens.Combinators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Data.Either
import           Data.Fixed
import           Data.List.Index
import           Data.Maybe
import           Data.Serialize
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Client
import           Network.Bitcoin.Api.Misc
import           Control.Concurrent.STM.TVar

import           Ergvein.Crypto.Hash
import           Ergvein.Filters.Btc.Mutable
import           Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.DB.Monad
import           Ergvein.Index.Server.DB.Schema.Filters
import           Ergvein.Index.Server.DB.Queries
import           Ergvein.Index.Server.DB.Utils
import           Ergvein.Index.Server.Dependencies
import           Ergvein.Index.Server.Monad
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Fees
import           Ergvein.Types.Transaction

import qualified Ergvein.Index.Protocol.Types       as IPT
import qualified Data.Set                           as Set
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Constants          as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HK


blockTxInfos :: (HasFiltersDB m, MonadLogger m) => HK.Block -> BlockHeight -> HK.Network -> m BlockInfo
blockTxInfos block txBlockHeight nodeNetwork = do
  let (txInfos , spentTxsIds) = mconcat $ txInfo <$> HK.blockTxns block

  uniqueSpentTxs <- mapM spentTxSource $ uniqueElements spentTxsIds
  blockAddressFilter <- fmap HK.encodeHex $ encodeBtcAddrFilter =<< makeBtcFilter nodeNetwork uniqueSpentTxs block
  let blockHeaderHashHexView = HK.blockHashToHex $ HK.headerHash $ HK.blockHeader block
      prevBlockHeaderHashHexView = HK.blockHashToHex $ HK.prevBlock $ HK.blockHeader block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHashHexView prevBlockHeaderHashHexView blockAddressFilter

  pure $ BlockInfo blockMeta spentTxsIds txInfos
  where
    blockTxMap = mapBy (HK.txHashToHex . HK.txHash) $ HK.blockTxns block
    spentTxSource :: (HasFiltersDB m, MonadLogger m) => TxHash -> m HK.Tx
    spentTxSource txInId =
      case Map.lookup txInId blockTxMap of
        Just    sourceTx -> pure sourceTx
        Nothing          -> fromChache
      where
        decodeError = "error decoding btc txIn source transaction " <> show txInId
        fromChache = do
          db <- getFiltersDb
          src <- getParsedExact "blockTxInfos" db $ txRecKey txInId
          pure $ fromRight (error decodeError) $ decode $ fromJust $ HK.decodeHex $ txRecHexView src

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

blockInfo :: (BitcoinApiMonad m,  HasBitcoinNodeNetwork m, HasFiltersDB m, MonadLogger m) => BlockHeight -> m BlockInfo
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

feeScaner :: ServerM ()
feeScaner = feeScaner' 0
  where
    feeScaner' :: BlockHeight -> ServerM ()
    feeScaner' h = do
      cfg <- serverConfig
      h'  <- actualHeight
      h'' <- if h' == h
        then pure h'
        else do
          res <- fmap catMaybes $ flip traverse [FeeFast, FeeModerate, FeeCheap] $ \lvl -> do
            let req mode c = estimateSmartFee c (fromIntegral $ feeTargetBlocks BTC lvl) mode
            mco <- nodeRpcCall $ req Conservative
            mec <- nodeRpcCall $ req Economical
            case (estimateResFee mco, estimateResFee mec) of
              (Just (MkFixed co), Just (MkFixed ec)) -> pure $ Just (lvl, (fromIntegral co `div` 1000 , fromIntegral ec `div` 1000))
              _ -> pure Nothing
          setFees IPT.BTC $ mkFeeBundle res
          logInfoN $ "[BTC]: " <> showt res
          pure $ case res of
            [] -> h
            _  -> h'
      shutdownFlagVar <- getShutdownFlag
      liftIO $ cancelableDelay shutdownFlagVar $ cfgFeeEstimateDelay cfg
      shutdownFlag <- liftIO $ readTVarIO shutdownFlagVar
      unless shutdownFlag $ feeScaner' h''
