module Ergvein.Wallet.Scan (
    scanner
  , scanBtcBlocks
  , refreshBtcKeys
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Time.Clock.POSIX
import Data.Vector (Vector)

import Ergvein.Filters.Btc.Mutable
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Filters.Loader
import Ergvein.Wallet.Log.Event
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC.Blocks
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Util

import qualified Data.List                          as L
import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Transaction        as HT

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found.
scanner :: MonadFront t m => m ()
scanner = do
  cursD <- getActiveCursD
  void $ widgetHoldDyn $ ffor cursD $ traverse_ $ \case
    BTC -> scannerBtc
    _ -> pure ()

scannerBtc :: forall t m . MonadFront t m => m ()
scannerBtc = void $ workflow checkRestored
  where
    filterReqDelay = 10
    -- | Before actually starting up check if the wallet is currently being restored
    -- if it is then wait until restore is done.
    checkRestored :: Workflow t m ()
    checkRestored = Workflow $ do
      logWrite "[scannerBtc][checkRestored]: Checking restore status"
      restoringE <- updatedWithInit . fmap _pubStorage'restoring =<< getPubStorageD
      let goE = listenHeight <$ ffilter not restoringE
      pure ((), goE)

    -- | Listen the btc network for new blocks
    -- Once we spot a new block has been issued, poll indexers for filters
    -- Repeat the request each filterReqDelay just in case
    listenHeight :: Workflow t m ()
    listenHeight = Workflow $ do
      logWrite "[scannerBtc][listenHeight]: Start"
      heightD <- getCurrentHeight BTC
      filterD <- getScannedHeightD BTC
      fhE <- updatedWithInit $ ((,) . fromIntegral) <$> heightD <*> filterD
      reqE <- widgetHoldE (pure never) $ ffor fhE $ \(h,f) -> if h >= f
        then do
          buildE <- getPostBuild
          te <- tickLossyFromPostBuildTime filterReqDelay
          let n = fromIntegral $ if f == h then 1 else h - f
          pure $ (f, n) <$ (leftmost [buildE, void te])
        else pure never
      filtersE <- getFilters BTC reqE
      pure ((), prepareBatch <$> filtersE)

    -- | Prepare the batch for processing
    -- Decode filters, add height info and get all the keys
    prepareBatch :: [(BlockHash, ByteString)] -> Workflow t m ()
    prepareBatch fs = Workflow $ do
      logWrite $ "[scannerBtc][prepareBatch]: Got a new batch of size " <> showt (length fs)
      fh <- getScannedHeight BTC
      let filts = zip [fh, fh+1 ..] fs
      batch <- fmap catMaybes $ flip traverse filts $ \(h, (bh, bs)) -> do
        efilt <- decodeBtcAddrFilter bs
        case efilt of
          Left err -> do
            logWrite $ "BTC filter decoding error: " <> (T.pack err)
            pure Nothing
          Right filt -> pure $ Just (h, bh, filt)
      psD <- getPubStorageD
      buildE <- getPostBuild
      publishStatusUpdate $ CurrencyStatus BTC (StatNewFilters $ length fs) <$ buildE
      nextE <- delay 0.1 $ ffor (current psD `tag` buildE) $ \ps -> let
        ext = repackKeys External $ pubStorageKeys BTC External ps
        int = repackKeys Internal $ pubStorageKeys BTC Internal ps
        nextHeight = fh + fromIntegral (length fs)
        in scanBatchKeys nextHeight batch $ (V.++) ext int
      pure ((), nextE)

    -- | Scan the batch against all provided keys
    -- After scanning and updating the wallet check if new keys have to be generated
    -- If there are new keys, feed them to scanBatchKeys again
    -- If there is none, set nextHeight and switch back to listenHeight
    scanBatchKeys :: BlockHeight -> [(BlockHeight, BlockHash, BtcAddrFilter)] -> V.Vector ScanKeyBox -> Workflow t m ()
    scanBatchKeys nextHeight batch keys = Workflow $ do
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
      buildE <- delay 0.1 =<< getPostBuild
      hashesE <- performFork $ ffor buildE $ const $ liftIO $ fmap catMaybes $
        flip traverse batch $ \(h, bh, filt) -> do
          let bhash = egvBlockHashToHk bh
          res <- applyBtcFilterMany bhash filt addrs
          pure $ if res then Just (bhash, fromIntegral h) else Nothing
      scanE <- scanBtcBlocks keys hashesE
      removedReplacedE <- removeTxsReplacedByFee =<< delay 1 (void scanE)
      keysE <- refreshBtcKeys removedReplacedE
      performEvent_ $ ffor keysE $ \ks ->
        logWrite $ "[scannerBtc][scanBatchKeys]: Scan done. Got " <> showt (V.length ks) <> " extra keys"
      let (nullE, extraE) = splitFilter V.null keysE
      goListenE <- setScannedHeightE BTC $ nextHeight <$ nullE
      let nextE = leftmost [listenHeight <$ goListenE, (scanBatchKeys nextHeight batch) <$> extraE]
      pure ((), nextE)

repackKeys :: KeyPurpose -> V.Vector EgvXPubKey -> V.Vector ScanKeyBox
repackKeys kp = V.imap $ \i k -> ScanKeyBox k kp i

-- | Check given blocks for transactions that are related to given set of keys and store txs into storage.
-- Return event that fires 'True' if we found any transaction and fires 'False' if not.
scanBtcBlocks :: MonadFront t m => Vector ScanKeyBox -> Event t [(HB.BlockHash, HB.BlockHeight)] -> m (Event t Bool)
scanBtcBlocks keys hashesE = do
  performEvent_ $ ffor hashesE $ \hs -> flip traverse_ hs $ \(_,a) -> logWrite $ showt a
  let noScanE = fforMaybe hashesE $ \bls -> if null bls then Just () else Nothing
  heightMapD <- holdDyn M.empty $ M.fromList <$> hashesE
  let rhashesE = fmap (nub . fst . unzip) $ hashesE
  _ <- logEvent "Blocks requested: " rhashesE
  blocksE <- requestBTCBlocks rhashesE
  storedBlocksE <- storeBlockHeadersE "scanBtcBlocks" BTC blocksE
  let blkHeightE = current heightMapD `attach` storedBlocksE
  txsUpdsE <- logEvent "Transactions got: " =<< getAddressesTxs ((\(height, blocks) -> (keys, height, blocks)) <$> blkHeightE)
  void $ insertTxsUtxoInPubKeystore "scanBtcBlocks" BTC txsUpdsE
  removeOutgoingTxs "scanBtcBlocks" BTC $ (M.elems . M.unions . V.toList . snd . V.unzip . fst) <$> txsUpdsE
  pure $ leftmost [(V.any (not . M.null . snd)) . fst <$> txsUpdsE, False <$ noScanE]

-- | Extract transactions that correspond to given address.
getAddressesTxs :: MonadFront t m
  => Event t (Vector ScanKeyBox, M.Map HB.BlockHash HB.BlockHeight, [HB.Block])
  -> m (Event t (Vector (ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate))
getAddressesTxs e = do
  psD <- getPubStorageD
  performFork $ ffor (current psD `attach` e) $ \(ps, (addrs, heights, blocks)) -> liftIO $ flip runReaderT ps $ do
    (vec, b) <- fmap V.unzip $ traverse (getAddrTxsFromBlocks heights blocks) addrs
    let (outs, ins) = V.unzip b
    let upds :: BtcUtxoUpdate = (M.unions $ V.toList outs, mconcat $ V.toList ins)
    pure (vec, upds)

-- | Finds all txs that should be replaced and removes them from storage.
-- Also stores information about transaction replacements in the storage.
-- Stage 1. See removeRbfTxsFromStorage1.
removeTxsReplacedByFee :: MonadFront t m => Event t () -> m (Event t ())
removeTxsReplacedByFee goE = do
  pubStorageD <- getPubStorageD
  replacedTxsE <- performFork $ ffor (tagPromptlyDyn pubStorageD goE) $ \ps -> do
    let btcps = ps ^. pubStorage'currencyPubStorages . at BTC . non (error $ "removeTxsReplacedByFee: BTC storage does not exist!")
        txStore = btcps ^. currencyPubStorage'transactions
        possiblyReplacedTxs = btcps ^. currencyPubStorage'possiblyReplacedTxs
    liftIO $ flip runReaderT txStore $ do
      confirmedTxIds <- M.keysSet <$> getConfirmedTxs
      let txsToRemove = getTxsToRemove confirmedTxIds possiblyReplacedTxs
      pure txsToRemove
  removedE <- removeRbfTxsFromStorage2 "removedReplacedTxs" $ (BTC,) <$> replacedTxsE
  pure removedE

getTxsToRemove ::
  Set TxId ->
  Map TxId (Set TxId) ->
  Set RemoveRbfTxsInfo
getTxsToRemove confirmedTxIds possiblyReplacedTxs =
  M.foldrWithKey' (helper confirmedTxIds) S.empty possiblyReplacedTxs
  where
    helper :: Set TxId -> TxId -> Set TxId -> Set RemoveRbfTxsInfo -> Set RemoveRbfTxsInfo
    helper confirmedTxIds possiblyReplacingTx possiblyReplacedTxs acc
      | possiblyReplacingTx `S.member` confirmedTxIds = S.insert (RemoveRbfTxsInfo possiblyReplacingTx possiblyReplacingTx possiblyReplacedTxs) acc
      | not $ S.null intersection = S.insert (RemoveRbfTxsInfo possiblyReplacingTx (S.findMin intersection) (S.insert possiblyReplacingTx (S.delete (S.findMin intersection) possiblyReplacedTxs))) acc
      | otherwise = acc
      where intersection = possiblyReplacedTxs `S.intersection` confirmedTxIds -- This intersection must contain only one element, because possiblyReplacedTxs are conflicting and no more than one tx may be valid

-- | Gets transactions related to given address from given block.
getAddrTxsFromBlocks :: (HasPubStorage m, PlatformNatives)
  => M.Map HB.BlockHash HB.BlockHeight
  -> [HB.Block]
  -> ScanKeyBox
  -> m ((ScanKeyBox, M.Map TxId EgvTx), BtcUtxoUpdate)
getAddrTxsFromBlocks heights blocks box = do
  (txMaps, uts) <- fmap unzip $ traverse (getAddrTxsFromBlock box heights) blocks
  let (outs,ins) = unzip uts
  let upds = (M.unions outs, mconcat ins)
  pure $ ((box, M.unions txMaps), upds)

getAddrTxsFromBlock :: (HasPubStorage m, PlatformNatives)
  => ScanKeyBox
  -> M.Map HB.BlockHash HB.BlockHeight
  -> HB.Block
  -> m (M.Map TxId EgvTx, BtcUtxoUpdate)
getAddrTxsFromBlock box heights block = do
  ps <- askPubStorage
  let origtxMap = ps ^. pubStorage'currencyPubStorages . at BTC . non (error "getAddrTxsFromBlock: BTC store does not exist") . currencyPubStorage'transactions
      newtxmap = M.fromList $ (\tx -> (mkTxId tx, BtcTx tx mheha)) <$> txs
      txmap = M.union newtxmap origtxMap
  liftIO $ flip runReaderT txmap $ do
    filteredTxs <- filterTxsForAddress addr txs
    let filteredIds = S.fromList $ mkTxId <$> filteredTxs
        filteredTxMap = M.restrictKeys newtxmap filteredIds
    utxo <- getUtxoUpdatesFromTxs mh box filteredTxs
    pure $ (filteredTxMap, utxo)
  where
    mkTxId = hkTxHashToEgv . HT.txHash
    addr = egvXPubKeyToEgvAddress $ scanBox'key box
    txs = HB.blockTxns block
    blockTime = secToTimestamp . HB.blockTimestamp $ HB.blockHeader $ block
    bhash = HB.headerHash . HB.blockHeader $ block
    mh = Just $ maybe 0 fromIntegral $ M.lookup bhash heights
    mheha = (\h -> EgvTxMeta (Just h) (Just bhash) blockTime) <$> mh
    secToTimestamp t = posixSecondsToUTCTime $ fromIntegral t

-- | Check if the gap is not enough and new keys are needed
-- generate new keys and return extra keys in an event to re-evaluate
refreshBtcKeys :: MonadFront t m => Event t () -> m (Event t (V.Vector ScanKeyBox))
refreshBtcKeys goE = do
  psD <- getPubStorageD
  ksVecE <- performFork $ ffor goE $ const $ do
    ps <- sampleDyn psD
    let masterPubKey = maybe (error "No BTC master key!") id $ pubStoragePubMaster BTC ps
    let ks = maybe (error "No BTC key storage!") id $ pubStorageKeyStorage BTC ps

    let eN = V.length $ pubStorageKeys BTC External ps
        eU = maybe 0 fst $ pubStorageLastUnusedKey BTC External ps
        extExtraNum = gapLimit - (eN - eU)
        extKeys = if extExtraNum > 0
          then derivePubKey masterPubKey External . fromIntegral <$> [eN .. eN+extExtraNum-1]
          else []
        extKb = zipWith (\i k -> ScanKeyBox k External i) [eN, eN+1 ..] extKeys
    let iN = V.length $ pubStorageKeys BTC Internal ps
        iU = maybe 0 fst $ pubStorageLastUnusedKey BTC Internal ps
        intExtraNum = gapLimit - (iN - iU)
        intKeys = if intExtraNum > 0
          then derivePubKey masterPubKey Internal . fromIntegral <$> [iN .. iN+intExtraNum-1]
          else []
        intKb = zipWith (\i k -> ScanKeyBox k Internal i) [iN, iN+1 ..] intKeys
    let vec = V.fromList $ extKb <> intKb
    pure $ case (extKeys, intKeys) of
      ([], []) -> (Nothing, vec)
      _ -> let
        ks' = foldl' (flip $ addXPubKeyToKeystore External) ks extKeys
        ks'' = foldl' (flip $ addXPubKeyToKeystore Internal) ks' intKeys
        in (Just ks'', vec)
  let storeE = fforMaybe ksVecE $ \(mks,_) -> case mks of
        Just ks -> Just $ Just . pubStorageSetKeyStorage BTC ks
        _ -> Nothing
  modifyPubStorage "deriveNewBtcKeys" storeE
  pure $ snd <$> ksVecE

-- | Calculate number of last restore key and gap that indicates how many unused
-- keys we are scanned already.
calcNumGap :: PubStorage -> Currency -> KeyPurpose -> Int
calcNumGap ps cur s = let
    l = V.length (pubStorageKeys cur s ps) - 1
    unused = maybe 0 fst $ pubStorageLastUnusedKey cur s ps
    in l - unused
