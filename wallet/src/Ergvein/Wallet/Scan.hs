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

-- | Check single key against unscanned filters and return 'True' if we found any tx (stored in public storage).
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
  txsE <- logEvent "Transactions got: " =<< getAddressesTxs ((keymap,) <$> blocksE)
  storedE <- insertTxsInPubKeystore $ (BTC,) . fmap M.keys <$> txsE
  pure $ leftmost [any (not . M.null) <$> txsE, False <$ noScanE]

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
