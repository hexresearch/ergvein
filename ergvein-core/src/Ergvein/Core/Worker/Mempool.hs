module Ergvein.Core.Worker.Mempool
  (
    btcMempoolWorker
  ) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Lens
import Ergvein.Core.Node.Btc ()
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Core.Store.Monad
import Ergvein.Text
import Ergvein.Types.Storage
import Network.Haskoin.Network hiding (Message(..))
import Network.Socket (SockAddr)
import Reflex
import Reflex.Workflow
import Reflex.Flunky
import Reflex.Fork
import Reflex.ExternalRef
import Sepulcas.Native
import Ergvein.Core.Wallet.Monad
import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Types
import Data.Serialize
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Ergvein.Filters.Btc.Mutable
import Ergvein.Core.Node.Manage (checkAddrMempoolTx)
import Ergvein.Core.Transaction
import Ergvein.Types.Utxo.Btc

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

maxFiltersRepeat :: Int
maxFiltersRepeat = 3

btcMempoolWorker :: forall t m . (MonadWallet t m, MonadNode t m) => m ()
btcMempoolWorker = do
  initConns <- getOpenSyncedConns BTC
  let initmap = M.fromList $ ffor initConns $ \c -> (indexConAddr c, c)
  let keys = M.keys initmap
  connsD <- externalRefDynamic =<< getActiveConnsRef
  pure ()

-- | Request a mempool from a random node
btcMempoolWorkerConn :: forall t m . (MonadWallet t m, MonadNode t m) => IndexerConnection t -> m ()
btcMempoolWorkerConn IndexerConnection{..} = void $ workflow waitRestore
  where
    waitRestore = Workflow $ do
      workLog "started"
      nextE <- updatedWithInit =<< (fmap . fmap) _pubStorage'restoring getPubStorageD
      pure ((), requestMempool 0 <$ nextE)
    requestMempool n = Workflow $ do
      buildE <- delay 5 =<< getPostBuild
      respE <- requestRandomIndexer $ (BTC, MGetMemFilters GetMemFilters) <$ buildE
      let memfiltersE = fforMaybe respE $ \(_, msg) -> case msg of
            MMemFilters (FilterTree ft) -> Just ft
            _ -> Nothing
      -- pure ((), scanFilters n <$> memfiltersE)
      pure ((), waitNextInv n <$ memfiltersE)
    scanFilters :: Int -> M.Map TxPrefix MempoolFilter -> Workflow t m ()
    scanFilters n filtTree = Workflow $ do
      workLog $ "fee tree size: " <> showt (M.size filtTree)
      ps <- getPubStorage
      let keys = getPublicKeys $ ps ^. btcPubStorage . currencyPubStorage'pubKeystore
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
      prefixes <- fmap catMaybes $ flip traverse (M.toList filtTree) $ \(pref, MempoolFilter bs) -> do
        efilt <- decodeBtcAddrFilter bs
        case efilt of
          Left err -> do
            workLog $ "BTC filter decoding error. [Prefix: " <> showt pref <> "]: " <> (T.pack err)
            pure Nothing
          Right filt -> do
            match <- applyBtcPrefixFilterMany pref filt addrs
            pure $ if match then Just pref else Nothing
      buildE <- getPostBuild
      workLog $ showt $ length prefixes
      pure $ ((), waitNextInv n <$ buildE)
      -- pure $ if 0 == length prefixes
      --   then ((), waitNextInv n <$ buildE)
      --   else ((), processChunks n prefixes <$ buildE)

    processChunks n prefixes = Workflow $ do
      workLog $ "Process prefixes: " <> showt prefixes
      ps <- getPubStorage
      let btcps = ps ^. btcPubStorage
          keys = getPublicKeys $ btcps ^. currencyPubStorage'pubKeystore
          txStore = btcps ^. currencyPubStorage'transactions

      buildE <- getPostBuild
      respE <- requestRandomIndexer $ (BTC, MGetMempool (GetMempool $ V.fromList prefixes)) <$ buildE
      let mempE = fforMaybe respE $ \(_, msg) -> case msg of
            MMempoolChunk (MempoolChunk pref txs) -> Just (pref, txs)
            _ -> Nothing
      valsE <- performFork $ ffor mempE $ \(pref, txs) -> do
        val <- fmap catMaybes $ flip traverse (V.toList txs) $ \txbs -> case decode txbs of
          Left err -> workLog "Error decoding tx" >> pure Nothing
          Right tx -> do
            -- This vvv reqires an Event. Rewrite in a non-reflexive manner
            -- removeTxsReplacedByFee tx
            liftIO $ flip runReaderT txStore $ do
              checkAddrTxResult <- checkAddrMempoolTx keys tx
              utxoUpdates <- getUtxoUpdates Nothing keys tx
              pure $ Just $ helper (checkAddrTxResult, utxoUpdates)
        pure val
      insertedE <- insertManyTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC valsE
      pure ((), waitNextInv n <$ insertedE)
      where
        helper :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
        helper ((vec, tx), utxoUpd) = ((, M.fromList [(egvTxId tx, tx)]) <$> vec, utxoUpd)

    waitNextInv :: Int ->  Workflow t m ()
    waitNextInv n = Workflow $ do
      workLog $ "waitNextInv: " <> showt n
      if n > maxFiltersRepeat then pure ((), never) else do
        buildE <- getPostBuild
        respE <- requestRandomIndexer $ (BTC, MPing 0) <$ buildE
        let filtInvE = fforMaybe respE $ \(_, msg) -> case msg of
              MFullFilterInv _ -> Just ()
              _ -> Nothing
        pure ((), requestMempool (n+1) <$ filtInvE)

workLog :: (PlatformNatives, MonadIO m) => Text -> m ()
workLog v = logWrite $ "[btcMempoolWorker]: " <> v

repackKeys :: KeyPurpose -> V.Vector EgvXPubKey -> V.Vector ScanKeyBox
repackKeys kp = V.imap $ \i k -> ScanKeyBox k kp i
