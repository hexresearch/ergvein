{-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Time
import Reflex.Localize.Dom

import Ergvein.Filters.Btc.Index
import Ergvein.Filters.Mutable hiding (BlockHeight)
import Ergvein.Text
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
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Timeout for trying to request filters again at 'getting filters batch' stage
filtersRetryTimeout :: NominalDiffTime
filtersRetryTimeout = 10

restorePage :: forall t m . MonadFront t m =>  m ()
restorePage = wrapperSimple True $ do
  void $ wipeRetract . (Nothing <$) =<< getPostBuild
  retractE <- retractEvent
  void $ nextWidget $ ffor retractE $ const $ Retractable (initialPage False) Nothing
  void $ workflow nodeConnection
  where
    restoreProgressWidget :: BlockHeight -> BlockHeight -> BlockHeight -> m ()
    restoreProgressWidget from to curr = do
      h2 $ localizedText $ RPSProgress p
      where
        [from', to', curr'] = fromIntegral <$> [from, to, curr]
        p = 100 * (curr' - from') / (to' - from')

    filtersBatchSize :: Int
    filtersBatchSize = 300

    -- | Stage 1: connect to BTC nodes
    nodeConnection = Workflow $ do
      h2 $ localizedText RPSConnecting
      statusBarWidget False BTC
      conmapD <- getNodesByCurrencyD BTC
      let upsD = fmap or $ join $ ffor conmapD $ \cm -> sequence $ ffor (M.elems cm) $ \case
            NodeConnBTC con -> nodeconIsUp con
            _ -> pure False
      let nextE = ffilter id $ updated upsD
      pure ((), heightAsking <$ nextE)

    -- | Stage 2: calculate the current height
    heightAsking = Workflow $ do
      h2 $ localizedText RPSGetHeight
      statusBarWidget False BTC
      heightD <- getCurrentHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      let heightE = leftmost [updated heightD, height0E]
      let nextE = fforMaybe heightE $ \h -> if h == 0 then Nothing else Just getFiltersBatch
      pure ((), nextE)

    -- | Stage 3: get a batch of filters and send them to stage 4
    -- if filters height >= btc height - 1, goto stage 5
    getFiltersBatch = Workflow $ do
      heightD <- getCurrentHeight BTC
      scannedHeight <- getScannedHeight BTC
      fullHeight <- fmap fromIntegral $ sampleDyn heightD
      let batchTipHeight = scannedHeight + fromIntegral filtersBatchSize
      restoreProgressWidget (filterStartingHeight BTC) fullHeight scannedHeight
      h3 $ localizedText RPSGetFiltsTitle
      h4 $ localizedText $ RPSGetFiltsFromTo scannedHeight $ if batchTipHeight > fullHeight then fullHeight else batchTipHeight
      psD <- getPubStorageD
      buildE <- delay 0.1 =<< getPostBuild
      tickE <- tickLossyFromPostBuildTime filtersRetryTimeout
      let checkE = leftmost [buildE, void tickE]
      let boolE = poke checkE $ const $ do
            h <- sampleDyn heightD
            pure $ fromIntegral scannedHeight >= (h - 1)
          (doneE, notDoneE) = splitFilter id boolE
      filtersE <- getFilters BTC $ (scannedHeight, filtersBatchSize) <$ notDoneE
      scanE <- performFork $ ffor filtersE $ \fs -> do
        let filts = zip [scannedHeight, scannedHeight+1 ..] fs
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
            nextHeight = scannedHeight + fromIntegral (length batch)
        pure $ scanBatchKeys (scannedHeight, nextHeight) batch keys
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
    scanBatchKeys (curHeight, nextHeight) batch keys = Workflow $ mdo
      (hE, cb) <- newTriggerEvent
      hD <- holdDyn curHeight hE
      fh <- sampleDyn . fmap fromIntegral =<< getCurrentHeight BTC
      restoreProgressWidget (filterStartingHeight BTC) fh curHeight
      let scanTitles = do
            h3 $ localizedText RPSScanTitle
            h4 $ localizedDynText $ RPSScanProgress <$> hD
          blockRetrieveTitles hashes = do
            h3 $ localizedText RPSBlocksTitle
            h4 $ localizedText $ RPSBlocskAmount (length hashes)
          refreshWalletTitles = h3 $ localizedText RPSKeysTitle
      widgetHold_ scanTitles $ leftmost [
          blockRetrieveTitles <$> hashesE
        , refreshWalletTitles <$ scanE
        ]
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
      buildE <- getPostBuild
      let chunks = mkChunks 100 batch
      let foo (h, bh, filt) = do
            liftIO $ when (h `mod` 10 == 0) $ do
              logWrite $ "Scanning " <> showt h
              cb h
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
      setE <- publishStatusUpdate $ CurrencyStatus BTC Synced <$ buildE
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
