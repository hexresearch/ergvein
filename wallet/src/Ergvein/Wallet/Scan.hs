module Ergvein.Wallet.Scan (
    scanner
  , scanningBtcKey
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe (fromMaybe, isNothing)
import Data.Vector (Vector)
import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Blocks.BTC
import Ergvein.Wallet.Blocks.Storage
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Log.Event
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Util

import qualified Data.IntMap.Strict                 as MI
import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Ergvein.Wallet.Filters.Scan        as Filters
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT

import Network.Haskoin.Address (addrToString)

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
    reconfirmBtxUtxoSet . updated . fmap fromIntegral =<< watchFiltersHeight BTC
    scannerBtc
  _ -> pure ()

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found. Specific for Bitcoin.
scannerBtc :: forall t m . MonadFront t m => m ()
scannerBtc = void $ workflow waiting
  where
    -- dbgStage = Workflow $ do
    --   buildE <- delay 0.1 =<< getPostBuild
    --   e <- writeWalletsScannedHeight $ (BTC, 1774599) <$ buildE
    --   pure ((), waiting <$ e)

    waiting = Workflow $ do
      logWrite "Waiting for unscanned filters"
      buildE <- getPostBuild
      fhD <- watchFiltersHeight BTC
      scD <- (fmap . fmap) fromIntegral $ getWalletsScannedHeightD BTC
      rsD <- fmap _pubStorage'restoring <$> getPubStorageD
      setSyncProgress $ ffor buildE $ const $ Synced
      let newFiltersE = fmapMaybe id $ updated $ do
            fh <- fhD
            sc <- scD
            rs <- rsD
            pure $ if sc < fh && not rs then Just (fh, sc) else Nothing
      setSyncProgress $ ffor newFiltersE $ \(fh, sc) -> do
        SyncMeta BTC (SyncBlocks 0 (fromIntegral (fh - sc))) 0 (fromIntegral (fh - sc))
      performEvent_ $ ffor newFiltersE $  \(fh, sc) -> do
        logWrite $ "Start scanning for new " <> showt (fh - sc)
      pure ((), scanning . snd <$> newFiltersE)

    scanning i0 = Workflow $ do
      logWrite "Scanning filters"
      waitingE <- scanningAllBtcKeys i0
      pure ((), waiting <$ waitingE)

-- | Check all keys stored in public storage agains unscanned filters and return 'True' if we found any tx (stored in public storage).
scanningAllBtcKeys :: MonadFront t m => HB.BlockHeight -> m (Event t Bool)
scanningAllBtcKeys i0 = do
  buildE <- getPostBuild
  ps <- getPubStorage
  let keys = getPublicKeys $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "scannerBtc: not exsisting store!") . currencyPubStorage'pubKeystore
  (updE, updFire) <- newTriggerEvent
  setSyncProgress updE
  let updSync i i1 = updFire $ SyncMeta BTC (SyncBlocks (fromIntegral (i - i0)) (fromIntegral (i1 - i0))) (fromIntegral (i - i0)) (fromIntegral (i1 - i0))
  scanE <- performFork $ ffor buildE $ const $ Filters.filterBtcAddresses i0 updSync $ xPubToBtcAddr . extractXPubKeyFromEgv <$> keys
  let heightE = fst <$> scanE
      hashesE = V.toList . snd <$> scanE
  writeWalletsScannedHeight $ ((BTC, ) . fromIntegral) <$> heightE
  performEvent_ $ ffor scanE $ \(h, bls) -> logWrite $ "Scanned all keys up to " <> showt h <> ", blocks to check: " <> showt bls
  scanningBtcBlocks (V.indexed keys) hashesE

-- | Check single key against unscanned filters and return 'True' if we found any tx (stored in public storage).
scanningBtcKey :: MonadFront t m => KeyPurpose -> HB.BlockHeight -> Int -> EgvXPubKey -> m (Event t Bool)
scanningBtcKey kp i0 keyNum pubkey = do
  (updE, updFire) <- newTriggerEvent
  setSyncProgress updE
  let sp = case kp of
        Internal -> SyncAddressInternal keyNum
        External -> SyncAddressExternal keyNum
  let updSync i i1 = if i `mod` 100 == 0
        then updFire $ SyncMeta BTC sp (fromIntegral (i - i0)) (fromIntegral (i1 - i0))
        else pure ()
      address = xPubToBtcAddr $ extractXPubKeyFromEgv pubkey
  buildE <- getPostBuild
  scanE <- performFork $ ffor buildE $ const $ Filters.filterBtcAddress i0 updSync $ xPubToBtcAddr . extractXPubKeyFromEgv $ pubkey
  let hashesE = V.toList . snd <$> scanE
  performEvent_ $ ffor scanE $ \(h, bls) -> logWrite $ "Scanned key #" <> showt keyNum <> " " <> btcAddrToString address <> " up to " <> showt h <> ", blocks to check: " <> showt bls
  scanningBtcBlocks (V.singleton (keyNum, pubkey)) hashesE

-- | Check given blocks for transactions that are related to given set of keys and store txs into storage.
-- Return event that fires 'True' if we found any transaction and fires 'False' if not.
scanningBtcBlocks :: MonadFront t m => Vector (Int, EgvXPubKey) -> Event t [(HB.BlockHash, HB.BlockHeight)] -> m (Event t Bool)
scanningBtcBlocks keys hashesE = do
  let noScanE = fforMaybe hashesE $ \bls -> if null bls then Just () else Nothing
  heightMapD <- holdDyn M.empty $ M.fromList <$> hashesE
  let rhashesE = fmap (nub . fst . unzip) $ hashesE
  _ <-logEvent "Blocks requested: " rhashesE
  blocksE <- requestBTCBlocks rhashesE
  storedBlocks <- storeMultipleBlocksTxHashesByE =<< storeMultipleBlocksByE blocksE
  let toAddr = xPubToBtcAddr . extractXPubKeyFromEgv
      keymap = M.fromList . V.toList . V.map (second (BtcAddress . toAddr)) $ keys
      blkHeightE = current heightMapD `attach` storedBlocks
  txsUpdsE <- logEvent "Transactions got: " =<< getAddressesTxs ((\(a,b) -> (keymap,a,b)) <$> blkHeightE)
  let txsE = fmap fst txsUpdsE
  let updE = fforMaybe txsUpdsE $ \(_,(o,i)) -> if not (M.null o && null i) then Just (o,i) else Nothing
  updateBtcUtxoSet updE
  storedE <- insertTxsInPubKeystore $ (BTC,) . fmap M.elems <$> txsE
  pure $ leftmost [any (not . M.null) <$> txsE, False <$ noScanE]

-- Left here for clarity
-- type BtcUtxoSet = M.Map OutPoint (Word64, EgvUtxoStatus)
--
-- type BtcUtxoUpdate = (BtcUtxoSet, [(OutPoint, Bool)])

-- | Extract transactions that correspond to given address.
getAddressesTxs :: MonadFront t m
  => Event t (M.Map Int EgvAddress, M.Map HB.BlockHash HB.BlockHeight, [HB.Block])
  -> m (Event t (M.Map Int (M.Map TxId EgvTx), BtcUtxoUpdate))
getAddressesTxs e = performFork $ ffor e $ \(maddr, heights, blocks) -> do
  a <- traverse (\a -> getAddrTxsFromBlocks a heights blocks) maddr
  let b = fmap fst a
  let (outs,ins) = unzip $ fmap snd $ M.elems a
  let upds :: BtcUtxoUpdate = (M.unions outs, mconcat ins)
  pure (b, upds)

-- | Gets transactions related to given address from given block.
getAddrTxsFromBlocks :: (MonadIO m, HasBlocksStorage m, PlatformNatives)
  => EgvAddress
  -> M.Map HB.BlockHash HB.BlockHeight
  -> [HB.Block]
  -> m (M.Map TxId EgvTx, BtcUtxoUpdate)
getAddrTxsFromBlocks addr heights blocks = do
  (txMaps, uts) <- fmap unzip $ traverse (getAddrTxsFromBlock addr heights) blocks
  let (outs,ins) = unzip uts
  let upds = (M.unions outs, mconcat ins)
  pure $ (M.unions txMaps, upds)

getAddrTxsFromBlock :: (MonadIO m, HasBlocksStorage m, PlatformNatives)
  => EgvAddress
  -> M.Map HB.BlockHash HB.BlockHeight
  -> HB.Block
  -> m (M.Map TxId EgvTx, BtcUtxoUpdate)
getAddrTxsFromBlock addr heights block = do
  checkResults <- traverse (checkAddrTx addr) txs
  let filteredTxs = fst $ unzip $ filter snd (zip txs checkResults)
  utxo <- getUtxoUpdatesFromTxs mh addr filteredTxs
  pure $ (, utxo) $ M.fromList [(HT.txHashToHex $ HT.txHash tx, BtcTx tx mheha) | tx <- filteredTxs]
  where
    txs = HB.blockTxns block
    bhash = HB.headerHash . HB.blockHeader $ block
    mh = Just $ maybe 0 fromIntegral $ M.lookup bhash heights
    mheha = (\h -> EgvTxMeta h bhash) <$> mh
