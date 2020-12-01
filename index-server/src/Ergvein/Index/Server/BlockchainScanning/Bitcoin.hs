module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Concurrent.Async.Lifted
import           Control.Lens.Combinators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.Either
import           Data.Fixed
import           Data.Maybe
import           Data.Serialize
import           Data.Text(Text)
import           Data.Time
import           Data.Word
import           Network.Bitcoin.Api.Blockchain
import           Network.Bitcoin.Api.Misc
import           Control.Concurrent.STM.TVar

import           Ergvein.Filters.Btc.Mutable
import           Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.Config
import           Ergvein.Index.Server.DB.Monad
import           Ergvein.Index.Server.DB.Schema.Filters
import           Ergvein.Index.Server.DB.Serialize
import           Ergvein.Index.Server.DB.Utils
import           Ergvein.Index.Server.Dependencies
import           Ergvein.Index.Server.Monad
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Fees
import           Ergvein.Types.Transaction
import qualified Data.ByteString                    as BS
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Ergvein.Index.Protocol.Types       as IPT
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK

blockTxInfos :: (HasFiltersDB m, MonadLogger m, MonadBaseControl IO m) => HK.Block -> BlockHeight -> m BlockInfo
blockTxInfos block txBlockHeight = do
  let (txInfos , spentTxsIds) = fmap (uniqueWithCount . mconcat) $ unzip $ txInfo <$> HK.blockTxns block
  -- timeLog $ "spentTxsIds: " <> showt (length spentTxsIds)
  uniqueSpentTxs <- fmap mconcat $ mapConcurrently (mapM spentTxSource) $ mkChunks 100 spentTxsIds
  blockAddressFilter <- encodeBtcAddrFilter =<<
    withInputTxs uniqueSpentTxs (makeBtcFilter isErgveinIndexable block)
  let blockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.headerHash $ HK.blockHeader block
      prevBlockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.prevBlock $ HK.blockHeader block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHash prevBlockHeaderHash blockAddressFilter

  pure $ BlockInfo blockMeta (Map.fromList spentTxsIds) txInfos
  where
    blockTxMap = mapBy (hkTxHashToEgv . HK.txHash) $ HK.blockTxns block
    spentTxSource :: (HasFiltersDB m, MonadLogger m) => (TxHash, Word32) -> m HK.Tx
    spentTxSource (txInId, _) = do
      case Map.lookup txInId blockTxMap of
        Just    sourceTx -> pure sourceTx
        Nothing          -> fromChache
      where
        fromChache = do
          db <- getFiltersDb
          src <- getParsedExact BTC "blockTxInfos" db $ txRawKey txInId
          case egvDeserialize BTC $ unTxRecBytes src of
            Left err -> error (err <> " : " <> show src)
            Right tx -> pure tx

    txInfo :: HK.Tx -> (TxInfo, [TxHash])
    txInfo tx = let
      withoutDataCarrier = none HK.isDataCarrier . HK.decodeOutputBS . HK.scriptOutput
      info = TxInfo { txHash = hkTxHashToEgv $ HK.txHash tx
                    , txBytes = egvSerialize BTC tx
                    , txOutputsCount = fromIntegral $ length $ filter withoutDataCarrier $  HK.txOut tx
                    }
      withoutCoinbaseTx = filter $ (/= HK.nullOutPoint)
      spentTxInfo = hkTxHashToEgv . HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
      in (info, spentTxInfo)

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount

blockInfo :: (BitcoinApiMonad m, HasFiltersDB m, MonadLogger m, MonadBaseControl IO m)
  => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan =  do
  blockHash <- nodeRpcCall $ (`getBlockHash` fromIntegral blockHeightToScan)
  conScheme <- getBtcConnectionScheme
  parsedBlock <- case conScheme of
    BtcConTCP -> requestBlock $ fromRight hashParsingError $ decode $ BS.reverse $ HS.toBytes blockHash
    BtcConRPC -> do
      maybeRawBlock <- nodeRpcCall $ (`getBlockRaw` blockHash)
      let rawBlock = fromMaybe blockParsingError maybeRawBlock
      pure $ fromRight blockGettingError $ decode $ HS.toBytes rawBlock
  blockTxInfos parsedBlock blockHeightToScan
  where
    hashParsingError = error $ "Error parsing BTC BlockHash at height " ++ show blockHeightToScan
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

timeLog :: (MonadLogger m, MonadIO m) => Text -> m ()
timeLog t = do
  now <- liftIO $ getCurrentTime
  logInfoN $ "["<> showt now <> "] " <> t
