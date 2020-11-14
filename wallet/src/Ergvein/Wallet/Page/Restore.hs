module Ergvein.Wallet.Page.Restore(
    restorePage
  ) where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe, catMaybes)
import Reflex.ExternalRef

import Ergvein.Filters.Btc.Index
import Ergvein.Filters.Mutable hiding (BlockHeight, BlockHash)
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Filters.Loader
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Scan
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Sync.Widget
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Transaction        as HT

restorePage :: forall t m . MonadFront t m =>  m ()
restorePage = wrapperSimple True $ void $ workflow nodeConnection
  where
    filtersBatchSize = 700
    -- | Stage 1: connect to BTC nodes
    nodeConnection = Workflow $ do
      el "h3" $ text "Connecting to nodes"
      syncWidget False BTC
      conmapD <- getNodesByCurrencyD BTC
      let upsD = fmap or $ join $ ffor conmapD $ \cm -> sequence $ ffor (M.elems cm) $ \case
            NodeConnBTC con -> nodeconIsUp con
            _ -> pure False
      let nextE = ffilter id $ updated upsD
      pure ((), heightAsking <$ nextE)

    -- | Stage 2: calculate the current height
    heightAsking = Workflow $ do
      el "h3" $ text "Getting current height"
      syncWidget False BTC
      heightD <- getCurrentHeight BTC
      height0E <- tag (current heightD) <$> getPostBuild
      let heightE = leftmost [updated heightD, height0E]
      let h0 = filterStartingHeight BTC
      let nextE = fforMaybe heightE $ \h -> if h == 0 then Nothing else Just (getFiltersBatch h0)
      pure ((), nextE)

    -- | Stage 3: get a batch of filters and send them to stage 4
    -- if filters height >= btc height - 1, goto stage 5
    getFiltersBatch fh = Workflow $ do
      h3 $ text $ "Getting a fresh batch of filters from: " <> showt fh
      psD <- getPubStorageD
      heightD <- getCurrentHeight BTC
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
        pure $ scanBatchKeys nextHeight batch keys
      finE <- delay 0.1 doneE
      let nextE = leftmost [finishScanning <$ finE, scanE]
      pure ((), nextE)

    -- | Stage 4: scan filters against given keys
    -- Afterwards check if new keys are generated
    -- then feed them to stage 4 with the same batch of filters
    -- else go back to stage 3 with the nextHeight
    scanBatchKeys :: BlockHeight -> [(BlockHeight, BlockHash, BtcAddrFilter)] -> V.Vector ScanKeyBox -> Workflow t m ()
    scanBatchKeys nextHeight batch keys = Workflow $ do
      (hE, cb) <- newTriggerEvent
      hD <- holdDyn "" hE
      h3 $ text "[scanBatchKeys] Scanning a batch ..."
      h4 $ text $ "[scanBatchKeys] Next batch starts at " <> showt nextHeight
      h4 $ dynText $ ffor hD $ \h -> "[scanBatchKeys] Scanning height: " <> h
      let mkAddr k = addressToScriptBS . xPubToBtcAddr . extractXPubKeyFromEgv $ scanBox'key k
      let addrs = V.toList $ mkAddr <$> keys
      buildE <- getPostBuild
      let chunks = mkChunks 100 batch
      let foo (h, bh, filt) = do
            liftIO $ cb $ showt h
            let bhash = egvBlockHashToHk bh
            res <- applyBtcFilterMany bhash filt addrs
            pure $ if res then Just (bhash, fromIntegral h) else Nothing

      hashesE <- performFork $ ffor buildE $ const $
        fmap (catMaybes . mconcat) $ liftIO $
          mapConcurrently (traverse foo) chunks
      scanE <- scanBtcBlocks keys hashesE
      extraE <- refreshBtcKeys $ void scanE
      let nextE = ffor extraE $ \extraKeys -> if V.null extraKeys
            then getFiltersBatch nextHeight
            else scanBatchKeys nextHeight batch extraKeys
      pure ((), nextE)

    -- Stage 5: finalize the restore and exit to the balances page
    finishScanning = Workflow $ do
      logWrite "Finished scanning BTC keys..."
      buildE <- getPostBuild
      setE <- setSyncProgress $ SyncProgress BTC Synced <$ buildE
      void $ modifyPubStorage "finishScanning" $ ffor setE $ const $ \ps -> Just $ ps {
          _pubStorage'restoring = False
        }
      _ <- nextWidget $ ffor buildE $ const $ Retractable {
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

-- | Calculate number of last restore key and gap that indicates how many unused
-- keys we are scanned already.
calcNumGap :: PubStorage -> Currency -> KeyPurpose -> Int
calcNumGap ps cur s = let
    l = V.length (pubStorageKeys cur s ps) - 1
    unused = maybe 0 fst $ pubStorageLastUnusedKey cur s ps
    in l - unused

-- | Check if the gap is not enough and new keys are needed
-- generate new keys and return extra keys in an event to re-evaluate
refreshBtcKeys :: MonadFront t m => Event t () -> m (Event t (V.Vector ScanKeyBox))
refreshBtcKeys goE = do
  psD <- getPubStorageD
  ksVecE <- performFork $ ffor goE $ const $ do
    ps <- sampleDyn psD
    let masterPubKey = maybe (error "No BTC master key!") id $ pubStoragePubMaster BTC ps
    let ks = maybe (error "No BTC key storage!") id $ pubStorageKeyStorage BTC ps

    let eN = V.length $ pubStorageKeys BTC External ps
        eU = maybe 0 fst $ pubStorageLastUnusedKey BTC External ps
        extExtraNum = gapLimit - (eN - eU)
        extKeys = if extExtraNum > 0
          then derivePubKey masterPubKey External . fromIntegral <$> [eN .. eN+extExtraNum-1]
          else []
        extKb = zipWith (\i k -> ScanKeyBox k External i) [eN, eN+1 ..] extKeys
    let iN = V.length $ pubStorageKeys BTC Internal ps
        iU = maybe 0 fst $ pubStorageLastUnusedKey BTC Internal ps
        intExtraNum = gapLimit - (iN - iU)
        intKeys = if intExtraNum > 0
          then derivePubKey masterPubKey Internal . fromIntegral <$> [iN .. iN+intExtraNum-1]
          else []
        intKb = zipWith (\i k -> ScanKeyBox k Internal i) [iN, iN+1 ..] intKeys
    let vec = V.fromList $ extKb <> intKb
    pure $ case (extKeys, intKeys) of
      ([], []) -> (Nothing, vec)
      _ -> let
        ks' = foldl' (flip $ addXPubKeyToKeystore External) ks extKeys
        ks'' = foldl' (flip $ addXPubKeyToKeystore Internal) ks' intKeys
        in (Just ks'', vec)
  let storeE = fforMaybe ksVecE $ \(mks,_) -> case mks of
        Just ks -> Just $ Just . pubStorageSetKeyStorage BTC ks
        _ -> Nothing
  modifyPubStorage "deriveNewBtcKeys" storeE
  pure $ snd <$> ksVecE


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
