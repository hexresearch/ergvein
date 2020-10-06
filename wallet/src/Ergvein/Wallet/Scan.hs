module Ergvein.Wallet.Scan (
    scanner
  , scanningBtcKey
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import Data.Time.Clock.POSIX
import Data.Vector (Vector)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Log.Event
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC.Blocks
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Util

import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Data.Vector                        as V
import qualified Ergvein.Wallet.Filters.Scan        as Filters
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Transaction        as HT

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found.
scanner :: MonadFront t m => m ()
scanner = do
  cursD <- getActiveCursD
  void $ widgetHoldDyn $ ffor cursD $ traverse_ scannerFor . S.toList

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found. Specific for currency.
scannerFor :: MonadFront t m => Currency -> m ()
scannerFor cur = case cur of
  BTC -> do
    reconfirmBtxUtxoSet "scannerFor" . updated . fmap fromIntegral =<< watchFiltersHeight BTC
    scannerBtc
  _ -> pure ()

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found. Specific for Bitcoin.
scannerBtc :: forall t m . MonadFront t m => m ()
scannerBtc = void $ workflow waiting
-- scannerBtc = void $ workflow dbgStage
  where
    -- dbgStage = Workflow $ do
    --   buildE <- delay 0.1 =<< getPostBuild
    --   rsD <- fmap _pubStorage'restoring <$> getPubStorageD
    --   let setE = ffilter not $ leftmost [updated rsD, tag (current rsD) buildE]
    --   e <- writeWalletsScannedHeight "setting dbg scan" $ (BTC, 640932) <$ setE
    --   pure ((), waiting <$ e)

    waiting = Workflow $ do
      logWrite "Waiting for unscanned filters"
      buildE <- getPostBuild
      fhD <- watchFiltersHeight BTC
      scD <- (fmap . fmap) fromIntegral $ getWalletsScannedHeightD BTC
      rsD <- fmap _pubStorage'restoring <$> getPubStorageD
      setSyncProgress $ ffor buildE $ const $ (BTC, Synced)
      let newFiltersE = fmapMaybe id $ updated $ do
            fh <- fhD
            sc <- scD
            rs <- rsD
            pure $ if sc < fh && not rs then Just (fh, sc) else Nothing
      -- setSyncProgress $ ffor newFiltersE $ \(fh, sc) -> do
      --   SyncMeta BTC (SyncBlocks 0 (fromIntegral (fh - sc))) 0 (fromIntegral (fh - sc))
      performEvent_ $ ffor newFiltersE $  \(fh, sc) -> do
        logWrite $ "Start scanning for new " <> showt (fh - sc)
      pure ((), scanning . snd <$> newFiltersE)

    scanning i0 = Workflow $ do
      logWrite "Scanning filters"
      waitingE <- scanningAllBtcKeys $ fromIntegral i0
      scD <- getWalletsScannedHeightD BTC
      cleanedE <- performFork $ ffor waitingE $ const $ do
        i1 <- fmap fromIntegral . sample . current $ scD
        logWrite $ "Cleaning filters from " <> showt i0 <> " to " <> showt i1
        clearFiltersRange BTC i0 i1
      pure ((), waiting <$ cleanedE)

-- | Check all keys stored in public storage agains unscanned filters and return 'True' if we found any tx (stored in public storage).
scanningAllBtcKeys :: forall t m . MonadFront t m => HB.BlockHeight -> m (Event t Bool)
scanningAllBtcKeys i0' = do
  let i0 = fromIntegral i0'
  buildE <- getPostBuild
  ps <- getPubStorage
  let keys = getPublicKeys $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "scannerBtc: not exsisting store!") . currencyPubStorage'pubKeystore
  (updE, updFire) <- newTriggerEvent
  setSyncProgress updE
  ch <- fmap fromIntegral . sample . current =<< getCurrentHeight BTC
  let updSync i i1 = updFire $ (BTC, ) $ SyncMeta BTC (SyncBlocks (fromIntegral i) ch) (fromIntegral $ i - i0) (ch - fromIntegral i0)
  scanE <- performFork $ ffor buildE $ const $ Filters.filterBtcAddresses i0 updSync $ xPubToBtcAddr . extractXPubKeyFromEgv . scanBox'key <$> keys
  let heightE = fst <$> scanE
      hashesE = ffor scanE $ \(_, vs) ->
        ffor (V.toList vs) $ \(ha,he) -> (egvBlockHashToHk ha, fromIntegral he)
      -- hashesE = V.toList . snd <$> scanE
      -- hashesE = never
  void $ writeWalletsScannedHeight "scanningAllBtcKeys" $ (BTC, ) <$> heightE
  performEvent_ $ ffor scanE $ \(h, bls) -> logWrite $ "Scanned all keys up to " <> showt h <> ", blocks to check: " <> showt bls
  scanningBtcBlocks keys hashesE

-- | Check single key against unscanned filters and return 'True' if we found any tx (stored in public storage).
scanningBtcKey :: MonadFront t m => KeyPurpose -> HB.BlockHeight -> Int -> EgvXPubKey -> m (Event t Bool)
scanningBtcKey kp i0' keyNum pubkey = do
  let i0 = fromIntegral i0'
  (updE, updFire) <- newTriggerEvent
  setSyncProgress updE
  let sp = case kp of
        Internal -> SyncAddressInternal keyNum
        External -> SyncAddressExternal keyNum
  let updSync i i1 = if i `mod` 100 == 0
        then updFire $ (BTC, ) $ SyncMeta BTC sp (fromIntegral (i - i0)) (fromIntegral (i1 - i0))
        else pure ()
      address = xPubToBtcAddr $ extractXPubKeyFromEgv pubkey
  buildE <- getPostBuild
  scanE <- performFork $ ffor buildE $ const $ Filters.filterBtcAddress i0 updSync $ xPubToBtcAddr . extractXPubKeyFromEgv $ pubkey
  let hashesE = ffor scanE $ \(_, vs) ->
        ffor (V.toList vs) $ \(ha,he) -> (egvBlockHashToHk ha, fromIntegral he)
  performEvent_ $ ffor scanE $ \(h, bls) -> logWrite $ "Scanned key #" <> showt keyNum <> " " <> btcAddrToString address <> " up to " <> showt h <> ", blocks to check: " <> showt bls
  scanningBtcBlocks (V.singleton (ScanKeyBox pubkey kp keyNum)) hashesE

-- | Check given blocks for transactions that are related to given set of keys and store txs into storage.
-- Return event that fires 'True' if we found any transaction and fires 'False' if not.
scanningBtcBlocks :: MonadFront t m => Vector ScanKeyBox -> Event t [(HB.BlockHash, HB.BlockHeight)] -> m (Event t Bool)
scanningBtcBlocks keys hashesE = do
  performEvent_ $ ffor hashesE $ \hs -> flip traverse_ hs $ \(_,a) -> logWrite $ showt a
  let noScanE = fforMaybe hashesE $ \bls -> if null bls then Just () else Nothing
  heightMapD <- holdDyn M.empty $ M.fromList <$> hashesE
  let rhashesE = fmap (nub . fst . unzip) $ hashesE
  _ <-logEvent "Blocks requested: " rhashesE
  blocksE <- requestBTCBlocks rhashesE
  storedBlocks <- storeBlockHeadersE "scanningBtcBlocks" BTC blocksE
  let blkHeightE = current heightMapD `attach` storedBlocks
  txsUpdsE <- logEvent "Transactions got: " =<< getAddressesTxs ((\(a,b) -> (keys,a,b)) <$> blkHeightE)
  void $ insertTxsUtxoInPubKeystore "scanningBtcBlocks" BTC txsUpdsE
  removeOutgoingTxs "scanningBtcBlocks" BTC $ (M.elems . M.unions . V.toList . snd . V.unzip . fst) <$> txsUpdsE
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
