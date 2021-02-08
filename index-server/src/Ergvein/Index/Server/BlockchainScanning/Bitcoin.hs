module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens.Combinators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Control.Parallel.Strategies
import           Data.Either
import           Data.Maybe
import           Data.Serialize
import           Data.Text(Text)
import           Data.Time
import           Data.Word
import           Network.Bitcoin.Api.Blockchain

import           Ergvein.Filters.Btc.Mutable
import           Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import           Ergvein.Index.Server.BlockchainScanning.Types
import           Ergvein.Index.Server.DB.Monad
import           Ergvein.Index.Server.DB.Schema.Utxo
import           Ergvein.Index.Server.DB.Schema.Indexer
import           Ergvein.Index.Server.DB.Schema.Filters
import           Ergvein.Index.Server.DB.Serialize
import           Ergvein.Index.Server.DB.Utils
import           Ergvein.Index.Server.DB.Queries
import           Ergvein.Index.Server.Dependencies
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Transaction

import qualified Data.Sequence                      as Seq
import qualified Data.ByteString                    as BS
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK

blockInfo :: (BitcoinApiMonad m, HasUtxoDB m, MonadLogger m, MonadBaseControl IO m, HasShutdownFlag m)
  => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan = blockTxInfos blockHeightToScan =<< getBtcBlockWithRepeat blockHeightToScan

blockTxInfos :: (HasShutdownFlag m, BitcoinApiMonad m, MonadBaseControl IO m, HasUtxoDB m, MonadLogger m, MonadBaseControl IO m) => BlockHeight -> HK.Block -> m BlockInfo
blockTxInfos txBlockHeight block = do
  let (txInfos , spentTxsIds) = fmap (uniqueWithCount . mconcat) $ unzip $ txInfo <$> HK.blockTxns block
  -- timeLog $ "spentTxsIds: " <> showt (length spentTxsIds)
  uniqueSpentTxs <- fmap mconcat $ mapConcurrently (mapM spentTxSource) $ mkChunks 100 spentTxsIds
  blockAddressFilter <- encodeBtcAddrFilter =<<
    withInputTxs uniqueSpentTxs (makeBtcFilter isErgveinIndexable block)
  let blockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.headerHash $ HK.blockHeader block
      prevBlockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.prevBlock $ HK.blockHeader block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHash prevBlockHeaderHash blockAddressFilter
  let spentTxsIdsMap = Map.mapKeys hkTxHashToEgv $ Map.fromList spentTxsIds
  pure $ BlockInfo blockMeta spentTxsIdsMap txInfos
  where
    blockTxMap = mapBy (HK.txHash) $ HK.blockTxns block
    spentTxSource :: (HasShutdownFlag m, MonadBaseControl IO m, BitcoinApiMonad m, HasUtxoDB m, MonadLogger m) => (HK.TxHash, Word32) -> m HK.Tx
    spentTxSource (txInId, _) = case Map.lookup txInId blockTxMap of
      Just    sourceTx -> pure sourceTx
      Nothing          -> do
        etx <- getTxFromCache $ hkTxHashToEgv txInId
        case etx of
          Left _ -> do
            logWarnN $ "[blockTxInfos]: Failed to get a Tx from DB. Trying the node. " <> showt txInId
            getTxFromNode txInId
          Right tx -> pure tx

    txInfo :: HK.Tx -> (TxInfo, [HK.TxHash])
    txInfo tx = let
      withoutDataCarrier = none HK.isDataCarrier . HK.decodeOutputBS . HK.scriptOutput
      info = TxInfo { txHash = hkTxHashToEgv $ HK.txHash tx
                    , txBytes = egvSerialize BTC tx
                    , txOutputsCount = fromIntegral $ length $ filter withoutDataCarrier $  HK.txOut tx
                    }
      withoutCoinbaseTx = filter $ (/= HK.nullOutPoint)
      spentTxInfo = HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
      in (info, spentTxInfo)

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount

getTxFromCache :: (HasUtxoDB m, MonadLogger m)
  => TxHash -> m (Either String HK.Tx)
getTxFromCache thash = do
  db <- getUtxoDb
  msrc <- getParsed BTC "getTxFromCache" db $ txBytesKey thash
  pure $ case msrc of
    Nothing -> Left $ "Tx not found. TxHash: " <> show thash
    Just src -> egvDeserialize BTC $ unTxRecBytes src

getTxFromNode :: (HasShutdownFlag m, BitcoinApiMonad m, MonadLogger m, MonadBaseControl IO m, HasUtxoDB m)
  => HK.TxHash -> m HK.Tx
getTxFromNode thash = do
  db <- getUtxoDb
  txHeight <- fmap unTxRecHeight $
    getParsedExact BTC "getTxFromNode" db $ txHeightKey $ hkTxHashToEgv thash
  blk <- getBtcBlockWithRepeat $ fromIntegral txHeight
  let txChunks = mkChunks 100 $ HK.blockTxns blk
  txs <- fmap mconcat $ mapConcurrently (pure . catMaybes . parMap rpar comparator) txChunks
  case txs of
    [] -> txGettingError txHeight
    tx:_ -> pure tx
  where
    comparator tx = if thash == HK.txHash tx then Just tx else Nothing
    txGettingError h = error $ "Failed to get tx from block #" ++ show h ++ " TxHash: " ++ show thash

getBtcBlockWithRepeat :: (BitcoinApiMonad m, MonadLogger m, MonadBaseControl IO m, MonadIO m, HasShutdownFlag m)
  => BlockHeight -> m HK.Block
getBtcBlockWithRepeat blockHeightReq = do
  resChan <- liftIO newTChanIO
  shutdownFlag <- getShutdownFlag
  fix $ \next -> do
    void $ fork $    -- Request thread
      liftIO . atomically . writeTChan resChan . Just =<< getBtcBlock blockHeightReq
    void $ fork $ do -- Timeout thread
      threadDelay 30000000 -- 30s
      liftIO . atomically . writeTChan resChan $ Nothing
    res <- liftIO $ atomically $ readTChan resChan
    case res of
      Nothing -> do
        b <- liftIO . readTVarIO $ shutdownFlag
        if b
          then throw $ ErrorCallWithLocation "Everything is fine, just killing the thread" "getBtcBlockWithRepeat"
          else next
      Just block -> pure block

getBtcBlock :: (BitcoinApiMonad m, MonadLogger m, MonadBaseControl IO m, MonadIO m)
  => BlockHeight -> m HK.Block
getBtcBlock blockHeightReq = do
  blockHash <- nodeRpcCall $ (`getBlockHash` fromIntegral blockHeightReq)
  conScheme <- getBtcConnectionScheme
  case conScheme of
    BtcConTCP -> requestBlock $ fromRight hashParsingError $ decode $ BS.reverse $ HS.toBytes blockHash
    BtcConRPC -> do
      maybeRawBlock <- nodeRpcCall $ (`getBlockRaw` blockHash)
      let rawBlock = fromMaybe blockParsingError maybeRawBlock
      pure $ fromRight blockGettingError $ decode $ HS.toBytes rawBlock
  where
    hashParsingError = error $ "Error parsing BTC BlockHash at height " ++ show blockHeightReq
    blockGettingError = error $ "Error getting BTC node at height " ++ show blockHeightReq
    blockParsingError = error $ "Error parsing BTC node at height " ++ show blockHeightReq

buildTxIndex :: (BitcoinApiMonad m, HasUtxoDB m, MonadLogger m, MonadBaseControl IO m, HasShutdownFlag m)
  => BlockHeight -> m TxIndexInfo
buildTxIndex blockHeightToScan = do
  block <- getBtcBlockWithRepeat blockHeightToScan
  let txIds = fmap (hkTxHashToEgv . HK.txHash) $ HK.blockTxns block
      blockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.headerHash $ HK.blockHeader block
      prevBlockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.prevBlock $ HK.blockHeader block
  pure $ TxIndexInfo blockHeightToScan blockHeaderHash prevBlockHeaderHash txIds


-- | Check database consistency
-- If the scanned height is Nothing -- delete all progress info just in case
-- and report that the db is consistent
-- Get the last scanned block from a node and check that:
-- a) there's a filter for that block
-- b) for each tx in the block there is at least the height info
-- If a or b is false -- report inconsistency
-- In addition check that the rollback sequence is consistent with the last scanned height
-- If not -- delete the sequence, we can afford to ignore it in almost every case
btcDbConsistencyCheck :: (BitcoinApiMonad m
  , HasUtxoDB m
  , HasFiltersDB m
  , HasIndexerDB m
  , HasBtcRollback m
  , MonadLogger m
  , MonadBaseControl IO m
  , HasShutdownFlag m) => m Bool
btcDbConsistencyCheck = do
  mh <- getScannedHeight BTC
  case mh of
    Nothing -> clearRollback >> deleteLastScannedBlock BTC >> pure True
    Just h -> do
      block <- getBtcBlockWithRepeat h
      let blockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.headerHash $ HK.blockHeader block
      mhash <- getLastScannedBlock BTC
      -- These inconsistencies can be repaired
      when (Just blockHeaderHash /= mhash) $ deleteLastScannedBlock BTC
      rse <- liftIO . readTVarIO =<< getBtcRollbackVar
      case rse of
        Seq.Empty -> pure ()
        tip Seq.:<| _ -> when (rollbackPrevHeight tip /= h - 1) clearRollback

      -- these cannot
      let txIds = fmap (hkTxHashToEgv . HK.txHash) $ HK.blockTxns block
      brec <- getBlockInfoRec BTC h
      case brec of
        Nothing -> pure False   -- Filter is missing
        Just (BlockInfoRec hh _) -> if blockHeaderHash /= hh
          then pure False       -- Filter is for the wrong block (how??)
          else do
            udb <- readUtxoDb
            checkTxs udb txIds  -- check that all transaction are stored
  where
    clearRollback = do
      storeRollbackSequence BTC $ RollbackSequence mempty
      rvar <- getBtcRollbackVar
      liftIO $ atomically $ writeTVar rvar mempty

    checkTxs _ [] = pure True
    checkTxs udb (th:xs) = do
      mheight :: Maybe Word32 <- getParsed BTC "consistencyCheck" udb $ txHeightKey th
      maybe (pure False) (const $ checkTxs udb xs) mheight

timeLog :: (MonadLogger m, MonadIO m) => Text -> m ()
timeLog t = do
  now <- liftIO $ getCurrentTime
  logInfoN $ "["<> showt now <> "] " <> t
