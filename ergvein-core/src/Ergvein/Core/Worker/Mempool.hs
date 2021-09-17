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
import Ergvein.Node.Constants

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

maxFiltersRepeat :: Int
maxFiltersRepeat = 3

data IndexerConnState t = ICSAdded (IndexerConnection t) | ICSDel

type IndexerMap t = M.Map ErgveinNodeAddr (IndexerConnection t)
type IndexerMMap t = M.Map ErgveinNodeAddr (Maybe (IndexerConnection t))

btcMempoolWorker :: forall t m . (MonadWallet t m, MonadNode t m) => m ()
btcMempoolWorker = do
  initConns <- getOpenSyncedConns BTC
  let initmap = M.fromList $ ffor initConns $ \c -> (indexConName c, c)
  let keys = M.keys initmap
  connsD <- externalRefDynamic =<< getActiveConnsRef
  cmD <- foldDyn mergeConns (initmap, mempty) (updated connsD)
  let updE = updated $ snd $ splitDynPure cmD
  listWithKeyShallowDiff initmap updE $ \_ ic _ ->
    btcMempoolWorkerConn ic
  pure ()
  where
    mergeConns :: IndexerMap t -> (IndexerMap t, IndexerMMap t) -> (IndexerMap t, IndexerMMap t)
    mergeConns new (old, _) = let
      add = fmap Just $ new M.\\ old
      del = fmap (const Nothing) $ old M.\\ new
      in (new, add `M.union` del)

-- | Request a mempool from a random node
btcMempoolWorkerConn :: forall t m . (MonadWallet t m, MonadNode t m) => IndexerConnection t -> m ()
btcMempoolWorkerConn IndexerConnection{..} = void $ workflow waitRestore
  where
    workLog :: Text -> m ()
    workLog v = logWrite $ "[btcMempoolWorker]<" <> indexConName <> ">: " <> v

    waitRestore :: Workflow t m ()
    waitRestore = Workflow $ do
      workLog "Started"
      nextE <- updatedWithInit =<< (fmap . fmap) _pubStorage'restoring getPubStorageD
      pure ((), waitIndexerUp <$ nextE)
    waitIndexerUp :: Workflow t m ()
    waitIndexerUp = Workflow $ do
      workLog "Waiting for indexer to be up"
      nextE <- fmap (ffilter id) $ updatedWithInit indexConIsUp
      pure ((), requestMempool 0 <$ nextE)

    requestMempool :: Int -> Workflow t m ()
    requestMempool n = Workflow $ do
      workLog "Indexer is up. Requesting mempool filters"
      buildE <- delay 5 =<< getPostBuild
      fire <- getIndexReqFire
      performEvent $ ffor buildE $ const $
        liftIO $ fire $ M.singleton indexConName (IndexerMsg $ MGetMemFilters GetMemFilters)
      let memfiltersE = fforMaybe indexConRespE $ \case
            MMemFilters (FilterTree ft) -> Just ft
            _ -> Nothing
      pure ((), scanFilters n <$> memfiltersE)

    scanFilters :: Int -> M.Map TxPrefix MempoolFilter -> Workflow t m ()
    scanFilters n filtTree = Workflow $ do
      workLog $ "Got fee tree of size: " <> showt (M.size filtTree)
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
            -- pure $ if match then Just pref else Nothing
            pure $ Just pref
      buildE <- eventToNextFrame =<< getPostBuild
      workLog $ showt $ length prefixes
      pure $ if null prefixes
        then ((), waitNextInv n <$ buildE)
        else ((), processChunks n prefixes <$ buildE)

    processChunks :: Int -> [TxPrefix] -> Workflow t m ()
    processChunks n prefixes = Workflow $ do
      workLog $ "Process " <> showt (length prefixes) <> " prefixes"
      ps <- getPubStorage
      let btcps = ps ^. btcPubStorage
          keys = getPublicKeys $ btcps ^. currencyPubStorage'pubKeystore
          txStore = btcps ^. currencyPubStorage'transactions

      buildE <- getPostBuild
      fire <- getIndexReqFire
      performEvent $ ffor buildE $ const $
        liftIO $ fire $ M.singleton indexConName (IndexerMsg $ MGetMempool (GetMempool $ V.fromList prefixes))
      let mempE = fforMaybe indexConRespE $ \case
            MMempoolChunk (MempoolChunk pref txs) -> Just (pref, txs)
            _ -> Nothing
      valsE <- performFork $ ffor mempE $ \(pref, txs) -> do
        val <- fmap catMaybes $ flip traverse (V.toList txs) $ \txbs -> case decode txbs of
          Left err -> do
            logWrite $ "[btcMempoolWorker]<" <> indexConName <> ">: " <> "Error decoding tx"
            pure Nothing
          Right tx -> do
            -- This vvv reqires an Event. Rewrite in a non-reflexive manner
            -- removeTxsReplacedByFee tx
            liftIO $ flip runReaderT txStore $ do
              mcheckAddrTxResult <- checkAddrMempoolTx keys tx
              case mcheckAddrTxResult of
                Nothing -> pure Nothing
                Just checkAddrTxResult -> do
                  utxoUpdates <- getUtxoUpdates Nothing keys tx
                  pure $ Just $ helper (checkAddrTxResult, utxoUpdates)
        pure val
      insertedE <- insertManyTxsUtxoInPubKeystore "btcMempoolTxInserter" BTC $ ffilter (not . null) valsE
      pure ((), waitNextInv n <$ insertedE)
      where
        helper :: ((V.Vector ScanKeyBox, EgvTx), BtcUtxoUpdate) -> (V.Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
        helper ((vec, tx), utxoUpd) = ((, M.fromList [(egvTxId tx, tx)]) <$> vec, utxoUpd)

    waitNextInv :: Int ->  Workflow t m ()
    waitNextInv n = Workflow $ do
      workLog $ "waitNextInv: " <> showt n
      if n >= maxFiltersRepeat then pure ((), never) else do
        let filtInvE = fforMaybe indexConRespE $ \case
              MFullFilterInv _ -> Just ()
              _ -> Nothing
        pure ((), requestMempool (n+1) <$ filtInvE)

repackKeys :: KeyPurpose -> V.Vector EgvXPubKey -> V.Vector ScanKeyBox
repackKeys kp = V.imap $ \i k -> ScanKeyBox k kp i
