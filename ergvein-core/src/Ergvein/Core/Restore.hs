{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Restore
  ( restore,
  )
where

import Control.Concurrent.Async
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import Data.Time
import Data.Traversable

import Ergvein.Core.Filters
import Ergvein.Core.Node
import Ergvein.Core.Status
import Ergvein.Core.Scan
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Filters.Btc.Index
import Ergvein.Filters.Btc.Mutable
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Reflex.Flunky
import Reflex.Fork
import Reflex.Main.Thread
import Reflex.Workflow
import Sepulcas.Native
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Timeout for trying to request filters again at 'getting filters batch' stage
filtersRetryTimeout :: NominalDiffTime
filtersRetryTimeout = 10

restore :: forall t m . (MonadHasMain m, MonadWallet t m, HasStatusEnv t m, HasNodeEnv t m, HasWalletEnv t m, HasStoreEnv t m) => m () -> m (Event t ())
restore renderRestorePage = do
  renderRestorePage
  workflowD <- workflow nodeConnection
  let isFinishedE = ffilter id (updated workflowD)
  pure $ () <$ isFinishedE
  where
    filtersBatchSize :: Int
    filtersBatchSize = 300

    nodeConnection = Workflow $ do
      -- Prevent the screen from turning off
      runOnMainThreadM androidSetScreenFlag
      logWrite "Stage 1. Waiting connection to at least one bitcoin node"
      buildE <- getPostBuild
      let status = def & walletStatusRestore'stage .~ RestoreStage'connectingToBtcNodes
      void $ updateWalletStatusRestore BTC $ const status <$ buildE
      conmapD <- getNodesByCurrencyD BTC
      let upsD = fmap or $
            join $
              ffor conmapD $ \cm -> sequence $
                ffor (M.elems cm) $ \case
                  NodeConnBtc con -> nodeconIsUp con
      let nextE = ffilter id $ updated upsD
      pure (False, heightAskingStage <$ nextE)

    heightAskingStage = Workflow $ do
      logWrite "Stage 2. Calculation of current height"
      heightD <- getCurrentHeight BTC
      scannedHeight <- getScannedHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      pubStorage <- getPubStorage
      let heightE = ffilter (/= 0) $ leftmost [updated heightD, height0E]
          mRestoreStartHeight = getRestoreStartHeight pubStorage
      case mRestoreStartHeight of
        Just _ -> do
          let nextE = getFiltersBatch (fromIntegral scannedHeight) <$ heightE
          pure (False, nextE)
        Nothing -> do
          restoreStartHeightSetE <- setRestoreStartHeightE $ scannedHeight <$ heightE
          let nextE = getFiltersBatch (fromIntegral scannedHeight) <$ restoreStartHeightSetE
          pure (False, nextE)

    getFiltersBatch :: BlockHeight -> Workflow t m Bool
    getFiltersBatch startHeight = Workflow $ do
      logWrite "Stage 3. Download filters"
      fullHeightD <- getCurrentHeight BTC
      scannedHeight <- getScannedHeight BTC
      psD <- getPubStorageD
      buildE <- delay 0.1 =<< getPostBuild
      tickE <- tickLossyFromPostBuildTime filtersRetryTimeout
      let checkE = leftmost [buildE, void tickE]
      let boolE = poke checkE $
            const $ do
              h <- sampleDyn fullHeightD
              pure $ fromIntegral scannedHeight >= (h - 1)
          (doneE, notDoneE) = splitFilter id boolE
      filtersE <- getFilters BTC $ (scannedHeight, filtersBatchSize) <$ notDoneE
      scanE <- performFork $
        ffor filtersE $ \fs -> do
          let filts = zip [scannedHeight, scannedHeight + 1 ..] fs
          batch <- fmap catMaybes $
            for filts $ \(h, (bh, bs)) -> do
              efilt <- decodeBtcAddrFilter bs
              case efilt of
                Left err -> do
                  logWrite $ "BTC filter decoding error: " <> T.pack err
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
      pure (False, nextE)
    scanBatchKeys ::
      BlockHeight ->
      (BlockHeight, BlockHeight) ->
      [(BlockHeight, BlockHash, BtcAddrFilter)] ->
      V.Vector ScanKeyBox ->
      Workflow t m Bool
    scanBatchKeys startHeight (curHeight, nextHeight) batch keys = Workflow $ mdo
      logWrite "Stage 4. Scan keys"
      buildE <- getPostBuild
      pubStorage <- getPubStorage
      fullHeight <- sampleDyn . fmap fromIntegral =<< getCurrentHeight BTC
      let
        mRestoreStartHeight = getRestoreStartHeight pubStorage
        restoreStartHeight = fromMaybe startHeight mRestoreStartHeight
        status fromH toH currH =
            def
              & walletStatusRestore'stage .~ RestoreStage'scanning
              & walletStatusRestore'progress ?~ calcPercentage fromH toH currH
      void $ updateWalletStatusRestore BTC $ const (status restoreStartHeight fullHeight curHeight) <$ buildE
      (_, cb) <- newTriggerEvent
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
      let chunks = mkChunks 100 batch
      let foo (h, bh, filt) = do
            liftIO $
              when (h `mod` 10 == 0) $ do
                logWrite $ "Scanning " <> showt h
                cb h
            let bhash = egvBlockHashToHk bh
            res <- applyBtcFilterMany bhash filt addrs
            pure $ if res then Just (bhash, fromIntegral h) else Nothing

      hashesE <-
        performFork $
          ffor buildE $
            const $
              fmap (catMaybes . mconcat) $
                liftIO $
                  mapConcurrently (traverse foo) chunks

      scanE <- scanBtcBlocks keys hashesE
      keysE <- refreshBtcKeys $ void scanE
      let (nullE, extraE) = splitFilter V.null keysE
      goNewBatchE <- setScannedHeightE BTC $ nextHeight <$ nullE
      let nextE = leftmost [getFiltersBatch startHeight <$ goNewBatchE, scanBatchKeys startHeight (curHeight, nextHeight) batch <$> extraE]
      pure (False, nextE)

    -- Stage 5: finalize the restore
    finishScanning :: Workflow t m Bool
    finishScanning = Workflow $ do
      -- | Allow the screen to turn off again
      runOnMainThreadM androidClearScreenFlag
      logWrite "Stage 5. Finalize restore"
      buildE <- getPostBuild
      setE <- updateWalletStatusNormal BTC $ const WalletStatusNormal'synced <$ buildE
      doneE <- delay 1 =<< modifyPubStorage "finishScanning" (
        ffor setE $
          const $ \ps ->
            Just $
              ps
                { _pubStorage'restoring = False
                })
      pure (False, exit <$ doneE)

    -- Stage 6: exit
    exit :: Workflow t m Bool
    exit = Workflow $ do
      pure (True, never)

repackKeys :: KeyPurpose -> V.Vector EgvXPubKey -> V.Vector ScanKeyBox
repackKeys kp = V.imap $ \i k -> ScanKeyBox k kp i
