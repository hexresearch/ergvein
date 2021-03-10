module Ergvein.Index.Server.BlockchainScanning.Bitcoin where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens.Combinators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
import           Data.ByteString(ByteString)
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
import           Ergvein.Index.Server.DB.Serialize
import           Ergvein.Index.Server.DB.Queries
import           Ergvein.Index.Server.Dependencies
import           Ergvein.Index.Server.Utils
import           Ergvein.Text
import           Ergvein.Types.Currency
import           Ergvein.Types.Transaction

import qualified Data.ByteString                    as BS
import qualified Data.HexString                     as HS
import qualified Data.Map.Strict                    as Map
import qualified Data.HashMap.Strict                as HM
import qualified Data.Serialize                     as S
import qualified Network.Haskoin.Block              as HK
import qualified Network.Haskoin.Crypto             as HK
import qualified Network.Haskoin.Script             as HK
import qualified Network.Haskoin.Transaction        as HK

type BlockInfoConstraints m = (
    BitcoinApiMonad m
  , MonadLogger m
  , MonadBaseControl IO m
  , HasShutdownSignal m
  , HasDbs m)

blockInfo :: BlockInfoConstraints m => BlockHeight -> m BlockInfo
blockInfo blockHeightToScan = blockTxInfos blockHeightToScan =<< getBtcBlockWithRepeat blockHeightToScan

blockTxInfos :: BlockInfoConstraints m => BlockHeight -> HK.Block -> m BlockInfo
blockTxInfos txBlockHeight block = do
  let (txInfos , spentTxsIds) = fmap (uniqueWithCount . mconcat) $ unzip $ txInfo <$> HK.blockTxns block
  -- timeLog $ "spentTxsIds: " <> showt (length spentTxsIds)
  vals <- fmap mconcat $ mapConcurrently (mapM spentTxSource) $ mkChunks 100 spentTxsIds
  let (spentUpds, uniqueSpentTxs) = unzip vals
  blockAddressFilter <- encodeBtcAddrFilter =<<
    withInputTxs uniqueSpentTxs (makeBtcFilter isErgveinIndexable block)
  let blockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.headerHash $ HK.blockHeader block
      prevBlockHeaderHash = HK.getHash256 $ HK.getBlockHash $ HK.prevBlock $ HK.blockHeader block
      blockMeta = BlockMetaInfo BTC txBlockHeight blockHeaderHash prevBlockHeaderHash blockAddressFilter
  let spentTxsIdsMap = HM.fromList spentUpds
  pure $ BlockInfo blockMeta spentTxsIdsMap txInfos
  where
    blockTxMap = mapBy (HK.txHash) $ HK.blockTxns block
    spentTxSource :: BlockInfoConstraints m => (HK.TxHash, Word32) -> m ((ByteString, Word32), HK.Tx)
    spentTxSource (txInId, sp) = case Map.lookup txInId blockTxMap of
      Just    sourceTx -> pure ((txIdBs, calcTxUnspent sourceTx), sourceTx)
      Nothing          -> do
        etx <- getTxFromCache txIdBs
        case etx of
          Left _ -> error $ "[blockTxInfos]: Failed to get a Tx from DB: " <> show txInId
          Right (tx, unsp) -> pure $ if sp >= unsp
            then ((txIdBs, 0), tx) else ((txIdBs, unsp - sp), tx)
      where txIdBs = S.encode $ hkTxHashToEgv txInId

    txInfo :: HK.Tx -> (TxInfo, [HK.TxHash])
    txInfo tx = let
      info = TxInfo { txHash = S.encode $ hkTxHashToEgv $ HK.txHash tx
                    , txBytes = serializeHkTx tx
                    , txOutputsCount = calcTxUnspent tx
                    }
      withoutCoinbaseTx = filter $ (/= HK.nullOutPoint)
      spentTxInfo = HK.outPointHash <$> (withoutCoinbaseTx $ HK.prevOutput <$> HK.txIn tx)
      in (info, spentTxInfo)

withoutDataCarrier :: HK.TxOut -> Bool
withoutDataCarrier = none HK.isDataCarrier . HK.decodeOutputBS . HK.scriptOutput
{-# INLINE withoutDataCarrier #-}

calcTxUnspent :: HK.Tx -> Word32
calcTxUnspent = fromIntegral . length . filter withoutDataCarrier . HK.txOut
{-# INLINE calcTxUnspent #-}

actualHeight :: (Monad m, BitcoinApiMonad m) => m BlockHeight
actualHeight = fromIntegral <$> nodeRpcCall getBlockCount

getTxFromCache :: (HasDbs m, MonadLogger m)
  => ByteString -> m (Either String (HK.Tx, Word32))
getTxFromCache thash = do
  msrc <- selectTxWithUnspent thash
  pure $ case msrc of
    Nothing -> Left $ "Tx not found. TxHash: " <> show thash
    Just (src, v) -> fmap (, v) $ deserializeHkTx src

getBtcBlockWithRepeat :: (BitcoinApiMonad m, MonadLogger m, MonadBaseControl IO m, MonadIO m, HasShutdownSignal m)
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

timeLog :: (MonadLogger m, MonadIO m) => Text -> m ()
timeLog t = do
  now <- liftIO $ getCurrentTime
  logInfoN $ "["<> showt now <> "] " <> t
