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
  BTC -> scannerBtc
  _ -> pure ()

-- | Widget that continuously scans new filters agains all known public keys and
-- updates transactions that are found. Specific for Bitcoin.
scannerBtc :: forall t m . MonadFront t m => m ()
scannerBtc = void $ workflow waiting
  where
    waiting = Workflow $ do
      logWrite "Waiting for unscanned filters"
      buildE <- getPostBuild
      fhD <- watchFiltersHeight BTC
      scD <- watchScannedHeight BTC
      rsD <- fmap _pubStorage'restoring <$> getPubStorageD
      setSyncProgress $ ffor buildE $ const $ Synced
      let newFiltersE = fmap fst . ffilter (id . snd) $ updated $ do
            fh <- fhD
            sc <- scD
            rs <- rsD
            pure (sc, sc < fh && not rs)
      setSyncProgress $ flip pushAlways newFiltersE $ const $ do
        fh <- sample . current $ fhD
        sc <- sample . current $ scD
        pure $ SyncMeta BTC (SyncAddress 0) (fromIntegral sc) (fromIntegral fh)
      performEvent_ $ ffor newFiltersE $ const $ do
        fh <- sample . current $ fhD
        sc <- sample . current $ scD
        logWrite $ "Start scanning for new " <> showt (fh - sc)
      pure ((), scanning <$> newFiltersE)

    scanning i0 = Workflow $  do
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
  let updSync i i1 = updFire $ SyncMeta BTC (SyncAddress (-1)) (fromIntegral (i - i0)) (fromIntegral (i1 - i0))
  scanE <- performFork $ ffor buildE $ const $ Filters.filterBtcAddresses updSync $ xPubToBtcAddr . extractXPubKeyFromEgv <$> keys
  let heightE = fst <$> scanE
      hashesE = V.toList . snd <$> scanE
  performFork_ $ writeScannedHeight BTC <$> heightE
  performEvent_ $ ffor scanE $ \(h, bls) -> logWrite $ "Scanned all keys up to " <> showt h <> ", blocks to check: " <> showt bls
  scanningBtcBlocks (V.indexed keys) hashesE

-- | Check all keys stored in public storage agains unscanned filters and return 'True' if we found any tx (stored in public storage).
scanningBtcKey :: MonadFront t m => HB.BlockHeight -> Int -> EgvXPubKey -> m (Event t Bool)
scanningBtcKey i0 keyNum pubkey = do
  (updE, updFire) <- newTriggerEvent
  setSyncProgress updE
  let updSync i i1 = if i `mod` 100 == 0
        then updFire $ SyncMeta BTC (SyncAddress keyNum) (fromIntegral (i - i0)) (fromIntegral (i1 - i0))
        else pure ()
  buildE <- getPostBuild
  scanE <- performFork $ ffor buildE $ const $ Filters.filterBtcAddress updSync $ xPubToBtcAddr . extractXPubKeyFromEgv $ pubkey
  let hashesE = V.toList . snd <$> scanE
  performEvent_ $ ffor scanE $ \(h, bls) -> logWrite $ "Scanned key # " <> showt keyNum <> " " <> showt pubkey <> " up to " <> showt h <> ", blocks to check: " <> showt bls
  scanningBtcBlocks (V.singleton (keyNum, pubkey)) hashesE

-- | Check given blocks for transactions that are related to given set of keys and store txs into storage.
-- Return event that fires 'True' if we found any transaction and fires 'False' if not.
scanningBtcBlocks :: MonadFront t m => Vector (Int, EgvXPubKey) -> Event t [HB.BlockHash] -> m (Event t Bool)
scanningBtcBlocks keys hashesE = do
  let noScanE = fforMaybe hashesE $ \bls -> if null bls then Just () else Nothing
  blocksE <- logEvent "Blocks requested: " =<< requestBTCBlocks hashesE
  storedBlocksE <- storeMultipleBlocksByE blocksE
  storedTxHashesE <- storeMultipleBlocksTxHashesByE blocksE
  let toAddr = xPubToBtcAddr . extractXPubKeyFromEgv
      keymap = M.fromList . V.toList . V.map (second (BtcAddress . toAddr)) $ keys
  txsE <- getAddressesTxs $ (keymap,) <$> blocksE
  storedE <- insertTxsInPubKeystore $ (BTC,) . fmap M.keys <$> txsE
  pure $ leftmost [not . M.null <$> txsE, False <$ noScanE]

type CurrentGap = Int
type AddressNum = Int

scanInternalAddressesByE :: MonadFront t m => Event t (Currency, CurrencyPubStorage) -> m (Event t (Currency, CurrencyPubStorage))
scanInternalAddressesByE = performEvent . fmap scanInternalAddresses

scanInternalAddresses :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => (Currency, CurrencyPubStorage) -> m (Currency, CurrencyPubStorage)
scanInternalAddresses (currency, currencyPubStorage) = do
  let startKeyIndex = 0
  newKeystore <- scanInternalAddressesLoop currencyPubStorage startKeyIndex 0
  pure (currency, newKeystore)

scanInternalAddressesLoop :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => CurrencyPubStorage -> Int -> Int -> m CurrencyPubStorage
scanInternalAddressesLoop currencyPubStorage keyIndex gap
  | gap == gapLimit = pure currencyPubStorage
  | otherwise = do
    let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
        masterPubKey = pubKeystore'master pubKeystore
        key = derivePubKey masterPubKey Internal (fromIntegral keyIndex)
        address = egvXPubKeyToEgvAddress key
        txs = currencyPubStorage ^. currencyPubStorage'transactions
        updatedPubKeystore = addXPubKeyToKeystore Internal key pubKeystore
        updatedPubStorage = currencyPubStorage & currencyPubStorage'pubKeystore .~ updatedPubKeystore
    logWrite $ "Scanning internal address #" <> showt keyIndex <> ": \"" <> egvAddrToString address <> "\""
    checkResults <- traverse (checkAddrTx address) $ egvTxsToBtcTxs txs -- TODO: use DMap instead of Map in CurrencyPubStorage
    if (M.size $ M.filter id checkResults) > 0
      then scanInternalAddressesLoop updatedPubStorage (keyIndex + 1) 0
      else scanInternalAddressesLoop updatedPubStorage (keyIndex + 1) (gap + 1)

-- TODO: This function will not be needed after using DMap as a CurrencyPubStorage
egvTxsToBtcTxs :: M.Map TxId EgvTx -> M.Map TxId BtcTx
egvTxsToBtcTxs egvTxMap = M.mapMaybe egvTxToBtcTx egvTxMap
  where egvTxToBtcTx tx = case tx of
          BtcTx t -> Just t
          _ -> Nothing

-- | If the given event fires and there is not fully synced filters. Wait for the synced filters and then fire the event.
waitFilters :: MonadFront t m => Currency -> Event t a -> m (Event t a)
waitFilters c e = mdo
  eD <- holdDyn Nothing $ leftmost [Just <$> e, Nothing <$ passValE']
  syncedD <- getFiltersSync c
  let passValE = fmapMaybe id $ updated $ foo <$> eD <*> syncedD
      notSyncE = attachWithMaybe
        (\b _ -> if b then Nothing else Just "Waiting filters sync...") (current syncedD) e
  performEvent_ $ logWrite <$> notSyncE
  passValE' <- delay 0.01 passValE
  pure passValE
  where
    foo :: Maybe a -> Bool -> Maybe a
    foo ma b = case (ma,b) of
      (Just a, True) -> Just a
      _ -> Nothing

-- | Gets a list of block hashes containing transactions related to given address.
filterAddress :: MonadFront t m => Event t EgvAddress -> m (Event t [HB.BlockHash])
filterAddress addrE = performFork $ ffor addrE Filters.filterAddress

-- | Gets transactions related to given address from given block list.
getTxs :: MonadFront t m => Event t (a, EgvAddress, [HB.Block]) -> m (Event t (a, M.Map TxId EgvTx))
getTxs = performFork . fmap (\(a, addr, bls) -> fmap (a,) $ getAddrTxsFromBlocks addr bls)

-- | Extract transactions that correspond to given address.
getAddressesTxs :: MonadFront t m => Event t (M.Map a EgvAddress, [HB.Block]) -> m (Event t (M.Map a (M.Map TxId EgvTx)))
getAddressesTxs e = performFork $ ffor e $ \(maddr, blocks) -> traverse (`getAddrTxsFromBlocks` blocks) maddr

-- | Gets transactions related to given address from given block.
getAddrTxsFromBlocks :: (MonadIO m, Traversable f, HasBlocksStorage m, PlatformNatives)
  => EgvAddress -> f HB.Block -> m (M.Map TxId EgvTx)
getAddrTxsFromBlocks addr blocks = do
  txMaps <- traverse (getAddrTxsFromBlock addr) blocks
  pure $ M.unions txMaps

getAddrTxsFromBlock :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HB.Block -> m (M.Map TxId EgvTx)
getAddrTxsFromBlock addr block = do
  checkResults <- traverse (checkAddrTx addr) txs
  let filteredTxs = fst $ unzip $ filter snd (zip txs checkResults)
  pure $ M.fromList [(HT.txHashToHex $ HT.txHash tx, BtcTx tx) | tx <- filteredTxs]
  where txs = HB.blockTxns block
