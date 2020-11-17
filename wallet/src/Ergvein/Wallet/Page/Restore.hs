{-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Reflex.Localize

import Ergvein.Filters.Btc.Index
import Ergvein.Filters.Mutable hiding (BlockHeight)
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Filters.Loader
import Ergvein.Wallet.Localization.Restore
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

restorePage :: forall t m . MonadFront t m =>  m ()
restorePage = wrapperSimple True $ void $ workflow nodeConnection
  where
    restoreProgressWidget :: BlockHeight -> BlockHeight -> BlockHeight -> m ()
    restoreProgressWidget f0' fh' hh' = h2 $ localizedText $ RPSProgress p
      where
        [f0, fh, hh] = fromIntegral <$> [f0',fh',hh']
        p = 100 * (fh - f0) / (hh - f0)

    filtersBatchSize :: Int
    filtersBatchSize = 700

    -- | Stage 1: connect to BTC nodes
    nodeConnection = Workflow $ do
      h2 $ localizedText RPSConnecting
      syncWidget False BTC
      conmapD <- getNodesByCurrencyD BTC
      let upsD = fmap or $ join $ ffor conmapD $ \cm -> sequence $ ffor (M.elems cm) $ \case
            NodeConnBTC con -> nodeconIsUp con
            _ -> pure False
      let nextE = ffilter id $ updated upsD
      pure ((), heightAsking <$ nextE)

    -- | Stage 2: calculate the current height
    heightAsking = Workflow $ do
      h2 $ localizedText RPSGetHeight
      syncWidget False BTC
      heightD <- getCurrentHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      let heightE = leftmost [updated heightD, height0E]
      let nextE = fforMaybe heightE $ \h -> if h == 0 then Nothing else Just getFiltersBatch
      pure ((), nextE)

    -- | Stage 3: get a batch of filters and send them to stage 4
    -- if filters height >= btc height - 1, goto stage 5
    getFiltersBatch = Workflow $ do
      heightD <- getCurrentHeight BTC
      fh <- getScannedHeight BTC
      hh <- fmap fromIntegral $ sampleDyn heightD
      let batchTipHeight = fh + fromIntegral filtersBatchSize
      restoreProgressWidget (filterStartingHeight BTC) fh hh
      h3 $ localizedText RPSGetFiltsTitle
      h4 $ localizedText $ RPSGetFiltsFromTo fh $ if batchTipHeight > hh then hh else batchTipHeight
      psD <- getPubStorageD
      buildE <- getPostBuild
      let boolE = poke buildE $ const $ do
            h <- sampleDyn heightD
            pure $ fromIntegral fh >= (h - 1)
          (doneE, notDoneE) = splitFilter id boolE
      filtersE <- getFilters BTC $ (fh, filtersBatchSize) <$ notDoneE
      scanE <- performFork $ ffor filtersE $ \fs -> do
        let filts = zip [fh, fh+1 ..] fs
        batch <- fmap catMaybes $ flip traverse filts $ \(h, (bh, bs)) -> do
          efilt <- decodeBtcAddrFilter bs
          case efilt of
            Left err -> do
              logWrite $ "BTC filter decoding error: " <> (T.pack err)
              pure Nothing
            Right filt -> pure $ Just (h, bh, filt)
        ps <- sampleDyn psD
        let ext = repackKeys External $ pubStorageKeys BTC External ps
            int = repackKeys Internal $ pubStorageKeys BTC Internal ps
            keys = (V.++) ext int
            nextHeight = fh + fromIntegral (length batch)
        pure $ scanBatchKeys (fh, nextHeight) batch keys
      finE <- delay 0.1 doneE
      let nextE = leftmost [finishScanning <$ finE, scanE]
      pure ((), nextE)

    -- | Stage 4: scan filters against given keys
    -- Afterwards check if new keys are generated
    -- then feed them to stage 4 with the same batch of filters
    -- else go back to stage 3 with the nextHeight
    scanBatchKeys :: (BlockHeight, BlockHeight)
      -> [(BlockHeight, BlockHash, BtcAddrFilter)]
      -> V.Vector ScanKeyBox
      -> Workflow t m ()
    scanBatchKeys (curHeight, nextHeight) batch keys = Workflow $ do
      (hE, cb) <- newTriggerEvent
      hD <- holdDyn curHeight hE
      hh <- sampleDyn . fmap fromIntegral =<< getCurrentHeight BTC
      restoreProgressWidget (filterStartingHeight BTC) curHeight hh
      h3 $ localizedText RPSScanTitle
      h4 $ localizedDynText $ RPSScanProgress <$> hD
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
      buildE <- getPostBuild
      let chunks = mkChunks 100 batch
      let foo (h, bh, filt) = do
            liftIO $ cb h
            let bhash = egvBlockHashToHk bh
            res <- applyBtcFilterMany bhash filt addrs
            pure $ if res then Just (bhash, fromIntegral h) else Nothing

      hashesE <- performFork $ ffor buildE $ const $
        fmap (catMaybes . mconcat) $ liftIO $
          mapConcurrently (traverse foo) chunks
      scanE <- scanBtcBlocks keys hashesE
      keysE <- refreshBtcKeys $ void scanE
      let (nullE, extraE) = splitFilter V.null keysE
      goNewBatchE <- setScannedHeightE BTC $ nextHeight <$ nullE
      let nextE = leftmost [getFiltersBatch <$ goNewBatchE, (scanBatchKeys (curHeight, nextHeight) batch) <$> extraE]
      pure ((), nextE)

    -- Stage 5: finalize the restore and exit to the balances page
    finishScanning = Workflow $ do
      buildE <- getPostBuild
      h2 $ localizedText RPSFinished
      setE <- setSyncProgress $ SyncProgress BTC Synced <$ buildE
      doneE <- modifyPubStorage "finishScanning" $ ffor setE $ const $ \ps -> Just $ ps {
          _pubStorage'restoring = False
        }
      nextE <- delay 1 doneE
      _ <- nextWidget $ ffor nextE $ const $ Retractable {
          retractableNext = balancesPage
        , retractablePrev = Nothing
        }
      pure ((), never)

repackKeys :: KeyPurpose -> V.Vector EgvXPubKey -> V.Vector ScanKeyBox
repackKeys kp = V.imap $ \i k -> ScanKeyBox k kp i

-- / Make chunks of length n
mkChunks :: Int -> [a] -> [[a]]
mkChunks n vals = mkChunks' [] vals
  where
     mkChunks' acc xs = case xs of
       [] -> acc
       _ -> let (a,b) = splitAt n xs in mkChunks' (acc ++ [a]) b
