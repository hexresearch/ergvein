module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Data.Foldable (foldl')

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper

import qualified Data.Vector as V

import Ergvein.Wallet.Debug

restorePage :: forall t m . MonadFront t m =>  m ()
restorePage = wrapperSimple True $ void $ workflow heightAsking
  where
    heightAsking = Workflow $ do
      debugWidget
      el "h3" $ text "Getting current height"
      heightD <- getCurrentHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      let heightE = leftmost [updated heightD, height0E]
      let nextE = fforMaybe heightE $ \h -> if h == 0 then Nothing else Just downloadFilters
      pure ((), nextE)

    downloadFilters = Workflow $ do
      el "h3" $ text "Downloading filters"
      filtersD <- watchFiltersHeight BTC
      heightD <- getCurrentHeight BTC
      el "h4" $ dynText $ do
        let h0 = filterStartingHeight BTC
        filters <- filtersD
        height <- heightD
        let pct = fromIntegral (filters - h0) / (fromIntegral $ fromIntegral height - h0) :: Float
        pure $ showf 2 (100 * pct) <> "%"
      filtersE <- fmap (ffilter id) $ updatedWithInit $ do
        filters <- filtersD
        height <- heightD
        pure $ filters >= fromIntegral height
      psD <- getPubStorageD
      let nextE = flip pushAlways filtersE $ const $ do
            ps <- sample . current $ psD
            let (er, egap) = calcNumGap ps BTC External
                (ir, igap) = calcNumGap ps BTC Internal
            pure $ if egap >= gapLimit
              then scanInternalKeys igap ir
              else scanKeys egap er
      performEvent_ $ ffor nextE $ const $ logWrite "Going to scan stage!"
      nextE' <- delay 0.1 nextE
      pure ((), nextE')

    scanKeys :: Int -> Int -> Workflow t m ()
    scanKeys gapN keyNum = Workflow $ do
      logWrite "We are at scan stage"
      syncWidget =<< getSyncProgress
      buildE <- delay 0.1 =<< getPostBuild
      keys <- pubStorageKeys BTC External <$> getPubStorage
      heightD <- getCurrentHeight BTC
      setSyncProgress $ flip pushAlways buildE $ const $ do
        h <- sample . current $ heightD
        pure $ SyncMeta BTC (SyncAddressExternal keyNum) 0 (fromIntegral h)
      if gapN >= gapLimit then do
        ps <- sample . current =<< getPubStorageD
        storedE <- modifyPubStorage "scanKeys" $ ffor buildE $ const $ Just . pubStorageSetKeyScanned BTC External (Just (keyNum + 1))
        let (ir, igap) = calcNumGap ps BTC Internal
        pure((), scanInternalKeys igap ir <$ storedE)
      else if keyNum >= V.length keys then do
        logWrite "Generating next portion of external BTC keys..."
        void $ deriveNewBtcKeys External gapLimit
        pure ((), scanKeys gapN keyNum <$ buildE)
      else do
        logWrite $ "Scanning external BTC key " <> showt keyNum
        h0 <- fmap fromIntegral . sample . current =<< getWalletsScannedHeightD BTC
        scannedE <- scanningBtcKey External h0 keyNum (keys V.! keyNum)
        hasTxsD <- holdDyn False scannedE
        storedE <- modifyPubStorage "scanKeys" $ ffor scannedE $ const $ Just . pubStorageSetKeyScanned BTC External (Just keyNum)
        let nextE = flip pushAlways storedE $ const $ do
              hastxs <- sample . current $ hasTxsD
              let gapN' = if hastxs then 0 else gapN+1
              pure $ scanKeys gapN' (keyNum+1)
        psD <- getPubStorageD
        performEvent_ $ ffor storedE $ const $ do
          hastxs <- sample . current $ hasTxsD
          when hastxs $ do
            ps <- sample . current $ psD
            logWrite $ "We have txs: " <> showt (pubStorageTxs BTC ps)
        nextE' <- delay 0.1 nextE
        pure ((), nextE')

    scanInternalKeys :: MonadFront t m => Int -> Int -> Workflow t m ()
    scanInternalKeys gapN keyNum = Workflow $ do
      buildE <- delay 0.1 =<< getPostBuild
      keys <- pubStorageKeys BTC Internal <$> getPubStorage
      syncWidget =<< getSyncProgress
      heightD <- getCurrentHeight BTC
      setSyncProgress $ flip pushAlways buildE $ const $ do
        h <- sample . current $ heightD
        pure $ SyncMeta BTC (SyncAddressInternal keyNum) 0 (fromIntegral h)
      if gapN >= gapLimit then pure ((), finishScanning <$ buildE)
      else if keyNum >= V.length keys then do
        logWrite "Generating next portion of internal BTC keys..."
        void $ deriveNewBtcKeys Internal gapLimit
        pure ((), scanInternalKeys gapN keyNum <$ buildE)
      else do
        logWrite $ "Scanning internal BTC key #" <> showt keyNum
        h0 <- fmap fromIntegral . sample . current =<< getWalletsScannedHeightD BTC
        scannedE <- scanningBtcKey Internal h0 keyNum (keys V.! keyNum)
        hasTxsD <- holdDyn False scannedE
        void $ modifyPubStorage "scanInternalKeys" $ ffor scannedE $ const $ Just . pubStorageSetKeyScanned BTC Internal (Just keyNum)
        let nextE = flip pushAlways scannedE $ const $ do
              hastxs <- sample . current $ hasTxsD
              let gapN' = if hastxs then 0 else gapN+1
              pure $ scanInternalKeys gapN' (keyNum + 1)
        nextE' <- delay 0.1 nextE
        pure ((), nextE')

    finishScanning = Workflow $ do
      logWrite "Finished scanning BTC keys..."
      buildE <- getPostBuild
      setSyncProgress $ Synced <$ buildE
      h <- sample . current =<< getCurrentHeight BTC
      scanhE <- writeWalletsScannedHeight "finishScanning" $ (BTC, fromIntegral h) <$ buildE
      clearedE <- performEvent $ clearFilters BTC <$ scanhE
      void $ modifyPubStorage "finishScanning" $ ffor clearedE $ const $ \ps -> Just $ ps {
          _pubStorage'restoring = False
        }
      _ <- nextWidget $ ffor buildE $ const $ Retractable {
          retractableNext = balancesPage
        , retractablePrev = Nothing
        }
      pure ((), never)

-- | Calculate number of last restore key and gap that indicates how many unused
-- keys we are scanned already.
calcNumGap :: PubStorage -> Currency -> KeyPurpose -> (Int, Int)
calcNumGap ps cur s = let
    r = pubStorageScannedKeys cur s ps
    unused = maybe 0 fst $ pubStorageLastUnused cur s ps
    gap = r - unused
  in (r, gap)

-- | Generate next public keys for bitcoin and put them to storage
deriveNewBtcKeys :: MonadFront t m => KeyPurpose -> Int -> m (Event t ())
deriveNewBtcKeys keyPurpose n = do
  buildE <- getPostBuild
  ps <- getPubStorage
  let keys = pubStorageKeys BTC keyPurpose ps
      keysN = V.length keys
      masterPubKey = maybe (error "No BTC master key!") id $ pubStoragePubMaster BTC ps
      newKeys = derivePubKey masterPubKey keyPurpose . fromIntegral <$> [keysN .. keysN+n-1]
      ks = maybe (error "No BTC key storage!") id $ pubStorageKeyStorage BTC ps
      ks' = foldl' (flip $ addXPubKeyToKeystore keyPurpose) ks newKeys
  modifyPubStorage "deriveNewBtcKeys" $ (Just . pubStorageSetKeyStorage BTC ks') <$ buildE
