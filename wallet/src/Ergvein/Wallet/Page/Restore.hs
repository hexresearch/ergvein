module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Ergvein.Text
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Ergvein.Filters.Btc
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Currencies
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Language
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.History
import Ergvein.Wallet.Page.PatternKey
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Worker.Node
import Ergvein.Wallet.Wrapper

import qualified Data.Vector as V

restorePage :: forall t m . MonadFront t m =>  m ()
restorePage = wrapperSimple True $ void $ workflow heightAsking
  where
    heightAsking = Workflow $ do
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
        filters <- filtersD
        height <- heightD
        let pct = fromIntegral filters / fromIntegral height :: Float
        -- pure $ showt filters <> "/" <> showt height <> " " <> showf 2 (100 * pct) <> "%"
        pure $ showf 2 (100 * pct) <> "%"
      filtersE <- fmap (ffilter id) $ updatedWithInit $ do
        filters <- filtersD
        height <- heightD
        pure $ filters >= fromIntegral height
      psD <- getPubStorageD
      let nextE = flip pushAlways filtersE $ const $ do
            ps <- sample . current $ psD
            let r = pubStorageScannedKeys BTC ps
                unused = maybe 0 fst $ pubStorageLastUnused BTC ps
                gap = r - unused
            pure $ scanKeys gap r
      pure ((), nextE)

    scanKeys :: Int -> Int -> Workflow t m ()
    scanKeys gapN keyNum = Workflow $ do
      syncWidget =<< getSyncProgress
      buildE <- delay 0.1 =<< getPostBuild
      keys <- pubStorageKeys BTC <$> getPubStorage
      heightD <- getCurrentHeight BTC
      setSyncProgress $ flip pushAlways buildE $ const $ do
        h <- sample . current $ heightD
        pure $ SyncMeta BTC (SyncAddress keyNum) 0 (fromIntegral h)
      if gapN >= gapLimit then pure ((), finishScanning <$ buildE)
      else if keyNum >= V.length keys then do
        logWrite "Generating next portion of BTC keys..."
        deriveNewBtcKeys gapLimit
        pure ((), scanKeys gapN keyNum <$ buildE)
      else do
        logWrite $ "Scanning BTC key " <> showt keyNum
        h0 <- sample . current =<< watchScannedHeight BTC
        scannedE <- scanningBtcKey h0 keyNum (keys V.! keyNum)
        let nextE = ffor scannedE $ \hastxs -> let
              gapN' = if hastxs then 0 else gapN+1
              in scanKeys gapN' (keyNum+1)
        modifyPubStorage $ ffor scannedE $ const $ Just . pubStorageSetKeyScanned BTC (Just keyNum)
        pure ((), nextE)

    finishScanning = Workflow $ do
      logWrite "Finished scanning BTC keys..."
      buildE <- getPostBuild
      h <- sample . current =<< getCurrentHeight BTC
      performFork_ $ writeScannedHeight BTC (fromIntegral h) <$ buildE
      modifyPubStorage $ ffor buildE $ const $ \ps -> Just $ ps {
          _pubStorage'restoring = False
        }
      _ <- nextWidget $ ffor buildE $ const $ Retractable {
          retractableNext = balancesPage
        , retractablePrev = Nothing
        }
      pure ((), never)

-- | Generate next public keys for bitcoin and put them to storage
deriveNewBtcKeys :: MonadFront t m => Int -> m (Event t ())
deriveNewBtcKeys n = do
  buildE <- getPostBuild
  ps <- getPubStorage
  let keys = pubStorageKeys BTC ps
      keysN = V.length keys
      masterPubKey = maybe (error "No BTC master key!") id $ pubStoragePubMaster BTC ps
      newKeys = derivePubKey masterPubKey External . fromIntegral <$> [keysN .. keysN+n-1]
      ks = maybe (error "No BTC key storage!") id $ pubStorageKeyStorage BTC ps
      ks' = foldl' (flip $ addXPubKeyToKeystore External) ks newKeys
  modifyPubStorage $ (Just . pubStorageSetKeyStorage BTC ks') <$ buildE
