module Ergvein.Index.Server.Bitcoin.Scanner
  (
    scanBlock
  ) where

import Control.Concurrent.Async.Lifted
import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Exception
import Control.Lens.Combinators (none)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Either
import Data.Maybe
import Data.Serialize
import Data.Word
import Network.Bitcoin.Api.Blockchain
import Network.Haskoin.Block (Block(..), BlockHeader(..), headerHash)
import Network.Haskoin.Crypto (getHash256)
import Network.Haskoin.Script (isDataCarrier, decodeOutputBS)
import Network.Haskoin.Transaction (
    OutPoint(..), Tx(..), TxIn(..), scriptOutput, nullOutPoint
  )

import Ergvein.Filters.Btc.Mutable
import Ergvein.Index.Server.Bitcoin.API
import Ergvein.Index.Server.DB.Queries
import Ergvein.Index.Server.DB.Serialize
import Ergvein.Index.Server.Monad.Class
import Ergvein.Index.Server.Types
import Ergvein.Index.Server.Utils
import Ergvein.Socket.Supervisor
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.ByteString                    as BS
import qualified Data.HashMap.Strict                as HM
import qualified Data.HexString                     as HS
import qualified Network.Haskoin.Block              as HK

scanBlock :: ServerMonad m => BlockHeight -> m BlockInfo
scanBlock height = processBlock height =<< getBtcBlockWithRepeat height

processBlock :: ServerMonad m => BlockHeight -> Block -> m BlockInfo
processBlock height block = do
  let txs = blockTxns block
  let (created, spent') = unzip $ extractPoints <$> txs
  let spent = mconcat spent'
  let createdMap = HM.fromList created
  -- lel <- mapM (spentPointScript createdMap) spent
  -- Tried replacing 🠗 with 🠕 Still leaks
  lel <- fmap mconcat $ mapConcurrently (mapM (spentPointScript createdMap)) $ mkEquisizedChunks 5 spent
  let hm = HM.fromList lel
  filt <- encodeBtcAddrFilter =<< withTxIndex hm (makeBtcFilter isErgveinIndexable block)
  let blockMeta = BlockMeta BTC height blockHeaderHash prevBlockHeaderHash filt
  -- tried forcing the whole line below. Still leaks
  pure $ BlockInfo blockMeta spent (removeDataCarriers created)
  where
    blockHeaderHash = getHash256 $ HK.getBlockHash $ headerHash $ blockHeader block
    prevBlockHeaderHash = getHash256 $ HK.getBlockHash $ prevBlock $ blockHeader block
    extractPoints :: Tx -> (TxInfo, [OutPoint])
    extractPoints tx = let
      thash = hkTxHash tx
      outs = zip [0..] $ fmap scriptOutput $ txOut tx
      txinfo = (thash, outs)
      spents = withoutCoinbaseTx $ prevOutput <$> txIn tx
      in (txinfo, spents)

    spentPointScript :: ServerMonad m => HM.HashMap ByteString [(Word32, ByteString)] -> OutPoint -> m (OutPoint, ByteString)
    spentPointScript hm op@(OutPoint th i) = do
      mbs <- getOutPointScript op
      case mbs of
        Nothing -> do
          let mtx = HM.lookup (encodeBtcTxHash th) hm
          let val = maybe Nothing (listToMaybe . filter ((==) i . fst)) mtx
          case val of
            Just (_, bs) -> pure (op,bs)
            Nothing -> error $ "Failed to get a script from DB for: " <> show op
        Just bs -> pure (op, bs)

withoutCoinbaseTx :: [OutPoint] -> [OutPoint]
withoutCoinbaseTx = filter $ (/= nullOutPoint)
{-# INLINE withoutCoinbaseTx #-}

removeDataCarriers :: [TxInfo] -> [TxInfo]
removeDataCarriers = fmap $ \(th,vals) ->
  (th, filter (none isDataCarrier . decodeOutputBS . snd) vals)
{-# INLINE removeDataCarriers #-}

data RequestStatus = RSResult Block | RSTimeout | RSTerminated

-- | This one was turned off and tested
-- I have left only getBtcBlock and it still leaked
getBtcBlockWithRepeat :: ServerMonad m => BlockHeight -> m Block
getBtcBlockWithRepeat blockHeightReq = do
  shutdownChan <- getShutdownChannel
  resChan <- liftIO newTChanIO
  let writeRes = liftIO . atomically . writeTChan resChan
  fix $ \restart -> do
    res <- withWorkersUnion $ \wu -> do
      spawnWorker wu $ fix $ \next -> do
        b <- liftIO $ atomically $ readTChan shutdownChan
        if b then writeRes RSTerminated else next
      spawnWorker wu $    -- Request thread
        writeRes . RSResult =<< getBtcBlock blockHeightReq
      spawnWorker wu $ do -- Timeout thread
        threadDelay 30000000 -- 30s
        writeRes RSTimeout
      liftIO $ atomically $ readTChan resChan
    case res of
      RSResult blk -> pure blk
      RSTimeout -> restart
      RSTerminated -> throw $ ErrorCallWithLocation "Everything is fine, just killing the thread" "getBtcBlockWithRepeat"


-- | I have once shortcutted this and was providing the same block
-- The leak persisted
getBtcBlock :: ServerMonad m => BlockHeight -> m Block
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
