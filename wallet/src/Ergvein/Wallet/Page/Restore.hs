{-# OPTIONS_GHC -Wall #-}
module Ergvein.Wallet.Page.Restore(
    restorePage
  , heightAskingProgressBounds
  ) where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import Data.Time

import Ergvein.Core.Status.Types
import Ergvein.Filters.Btc.Index
import Ergvein.Filters.Mutable hiding (BlockHeight)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Status.Widget
import Ergvein.Wallet.Util
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Timeout for trying to request filters again at 'getting filters batch' stage
filtersRetryTimeout :: NominalDiffTime
filtersRetryTimeout = 10

heightAskingProgressBounds :: (Double, Double)
heightAskingProgressBounds = (0, 5)

blockScanningProgressBounds :: (Double, Double)
blockScanningProgressBounds = (5, 100)

restorePage :: forall t m . MonadFront t m =>  m ()
restorePage = wrapperSimpleLogout True $ do
  restoreStatusWidget BTC
  -- restoreStatusDebugWidget BTC
  void $ workflow nodeConnection
  where

    filtersBatchSize :: Int
    filtersBatchSize = 300

    -- | Stage 1: connect to BTC nodes
    nodeConnection = Workflow $ do
      buildE <- getPostBuild
      let status = def
            & walletStatusRestore'stage .~ RestoreStage'connectingToBtcNodes
            & walletStatusRestore'progress .~ Nothing
      void $ updateWalletStatusRestore BTC $ (const status) <$ buildE
      conmapD <- getNodesByCurrencyD BTC
      let upsD = fmap or $ join $ ffor conmapD $ \cm -> sequence $ ffor (M.elems cm) $ \case
            NodeConnBtc con -> nodeconIsUp con
            _ -> pure False
      let nextE = ffilter id $ updated upsD
      pure ((), heightAskingStage <$ nextE)

    -- | Stage 2: calculate the current height
    heightAskingStage = Workflow $ do
      heightD <- getCurrentHeight BTC
      scannedHeight <- getScannedHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      let heightE = leftmost [updated heightD, height0E]
      let nextE = fforMaybe heightE $ \h -> if h == 0 then Nothing else Just $ getFiltersBatch $ fromIntegral scannedHeight
      pure ((), nextE)

    -- | Stage 3: get a batch of filters and send them to stage 4
    -- if filters height >= btc height - 1, goto stage 5
    getFiltersBatch :: BlockHeight -> Workflow t m ()
    getFiltersBatch startHeight = Workflow $ do
      fullHeightD <- getCurrentHeight BTC
      scannedHeight <- getScannedHeight BTC
      psD <- getPubStorageD
      buildE <- delay 0.1 =<< getPostBuild
      tickE <- tickLossyFromPostBuildTime filtersRetryTimeout
      let checkE = leftmost [buildE, void tickE]
      let boolE = poke checkE $ const $ do
            h <- sampleDyn fullHeightD
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
        pure $ scanBatchKeys startHeight (scannedHeight, nextHeight) batch keys
      finE <- delay 0.1 doneE
      let nextE = leftmost [finishScanning <$ finE, scanE]
      pure ((), nextE)

    -- | Stage 4: scan filters against given keys
    -- Afterwards check if new keys are generated
    -- then feed them to stage 4 with the same batch of filters
    -- else go back to stage 3 with the nextHeight
    scanBatchKeys ::
         BlockHeight
      -> (BlockHeight, BlockHeight)
      -> [(BlockHeight, BlockHash, BtcAddrFilter)]
      -> V.Vector ScanKeyBox
      -> Workflow t m ()
    scanBatchKeys startHeight (curHeight, nextHeight) batch keys = Workflow $ mdo
      buildE <- getPostBuild
      let status from to curr = def
            & walletStatusRestore'stage .~ RestoreStage'scanning
            & walletStatusRestore'progress .~ Just (mapPercentage blockScanningProgressBounds $ calcPercentage from to curr)
      void $ updateWalletStatusRestore BTC $ (const $ status startHeight fullHeight curHeight) <$ buildE
      (_, cb) <- newTriggerEvent
      fullHeight <- sampleDyn . fmap fromIntegral =<< getCurrentHeight BTC
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
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
      let nextE = leftmost [getFiltersBatch startHeight <$ goNewBatchE, (scanBatchKeys startHeight (curHeight, nextHeight) batch) <$> extraE]
      pure ((), nextE)

    -- Stage 5: finalize the restore and exit to the balances page
    finishScanning = Workflow $ do
      buildE <- getPostBuild
      setE <- updateWalletStatusNormal BTC $ (const WalletStatusNormal'synced) <$ buildE
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
