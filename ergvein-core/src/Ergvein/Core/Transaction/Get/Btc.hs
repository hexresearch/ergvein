{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Get.Btc(
    getTransactionsBtc
) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe (fromMaybe, fromJust, isJust)
import Data.Time
import Data.Word
import Network.Haskoin.Address
import Reflex.Flunky
import Reflex.Fork
import Reflex.Main.Thread

import Ergvein.Core.Node.Monad
import Ergvein.Core.Settings
import Ergvein.Core.Store.Util
import Ergvein.Core.TimeZone
import Ergvein.Core.Transaction.Btc
import Ergvein.Core.Transaction.Util.Common
import Ergvein.Core.Transaction.View.Common
import Ergvein.Core.Transaction.View.Btc
import Ergvein.Core.Wallet.Monad
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Transaction
import Sepulcas.Native

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Network.Haskoin.Block          as HK
import qualified Network.Haskoin.Transaction    as HK

txListRaw ::
  [Maybe HK.BlockHeader] ->
  [Maybe HK.BlockHash] ->
  [BtcTx] ->
  [Money] ->
  [Bool] ->
  [[TransOutputType]] ->
  [[Maybe HK.TxOut]] ->
  [[TxId]] ->
  [[TxId]] ->
  [(Bool, [TxId])] ->
  [TxInfo]
txListRaw (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) (j:js) = TxInfo a b c d e f g h i j : txListRaw as bs cs ds es fs gs hs is js
txListRaw _ _ _ _ _ _ _ _ _ _ = []

getTransactionsBtc :: (MonadWallet t m, MonadSettings t m, PerformMain t m, MonadNode t m) => m (Dynamic t [TxView], Dynamic t Word64)
getTransactionsBtc = do
  buildE <- delay 0.2 =<< getPostBuild
  settings <- getSettings
  pubStorageD <- getPubStorageD
  heightD <- getCurrentHeight BTC
  let allBtcAddrsD = getAllBtcAddrs <$> pubStorageD
  timeZoneE <- performTimeZone buildE
  timeZoneD <- holdDyn utc timeZoneE
  let txListD = ffor pubStorageD getBtcTxs
      filterE = leftmost [tagPromptlyDyn txListD timeZoneE, updated txListD]
  filteredTxListE <- performFork $ ffor filterE $ \txs -> do
    timeZone <- sampleDyn timeZoneD
    pubStorage <- sampleDyn pubStorageD
    getAndFilterBlocks heightD allBtcAddrsD timeZone txs pubStorage settings
  filteredTxListD <- holdDyn [] filteredTxListE
  pure (filteredTxListD, heightD)

getAndFilterBlocks :: (HasPubStorage (ReaderT r IO), MonadIO m, Reflex t, PlatformNatives, MonadSample t m) =>
  Dynamic t BlockHeight ->
  Dynamic t [BtcAddress] ->
  TimeZone ->
  [BtcTx] ->
  r ->
  Settings ->
  m [TxView]
getAndFilterBlocks heightD btcAddrsD timeZone txs store settings = do
  allBtcAddrs <- sampleDyn btcAddrsD
  hght <- sampleDyn heightD
  liftIO $ flip runReaderT store $ do
    let txHashes = HK.txHash . getBtcTx <$> txs
        txsRefList = fmap (calcRefill allBtcAddrs) txs
        parentTxsIds = fmap
          (fmap (hkTxHashToEgv . HK.outPointHash . HK.prevOutput)
             . HK.txIn . getBtcTx)
          txs
    blh <- traverse getBtcBlockHashByTxHash txHashes
    bl <- traverse (maybe (pure Nothing) getBlockHeaderByHash) blh
    pubStorage <- askPubStorage
    txStore <- getTxStorage BTC
    flip runReaderT txStore $ do
      bInOut <- traverse (checkAddrInOut allBtcAddrs) txs
      parentTxs <- traverse (traverse getTxById) parentTxsIds
      txStorage <- askTxStorage
      let storedTxs = fromJust . toTxBtc <$> M.elems txStorage
      outsStatuses <- traverse (getOutsStatuses storedTxs allBtcAddrs) txs
      spentOuts <- traverse (\tx -> getOutputsByOutPoints $ HK.prevOutput <$> (HK.txIn . getBtcTx $ tx)) txs
      let btcPubStorageMeta = fromMaybe (error "getAndFilterBlocks: BTC storage does not exist!") $ pubStorage ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc
          replacedTxsStore = btcPubStorageMeta ^. btcPubStorage'replacedTxs
          possiblyReplacedTxsStore = btcPubStorageMeta ^. btcPubStorage'possiblyReplacedTxs
          replacedTxs = fmap BtcTxHash <$> getReplacedTxs replacedTxsStore txs
          possiblyReplacedTxs = getPossiblyReplacedTxs possiblyReplacedTxsStore txs
          conflictingTxs = fmap BtcTxHash <$> getConflictingTxs possiblyReplacedTxs (getBtcTx <$> txs) -- This might be inefficient, better to calculate this only for unconfirmed txs
          getTxConfirmations mTx = case mTx of
            Nothing -> 1 -- If tx is not found in our storage we prefer to treat it as confirmed
            Just tx -> maybe 0 (countConfirmations hght . etxMetaHeight) (getBtcTxMeta $ fromJust . toTxBtc $ tx)
          txParentsConfirmations = (fmap . fmap) getTxConfirmations parentTxs
          hasUnconfirmedParents = fmap (elem 0) txParentsConfirmations -- This might be inefficient, better to calculate this only for unconfirmed txs
          explorerUrls = btcSettings'explorerUrls $ getBtcSettings settings
          rawTxsL = L.filter (\(a,_) -> isJust a) $ L.zip bInOut $ txListRaw bl blh txs txsRefList hasUnconfirmedParents outsStatuses spentOuts conflictingTxs replacedTxs ((fmap . fmap . fmap) BtcTxHash possiblyReplacedTxs)
          prepTxs = L.sortOn txView'time $ prepareTxView allBtcAddrs hght timeZone explorerUrls <$> rawTxsL
      pure $ L.reverse $ addWalletState prepTxs

calcRefill :: Foldable t => t BtcAddress -> BtcTx -> Money
calcRefill ac (BtcTx btx _) = Money BTC $ sum $ fmap (HK.outValue . snd) $ L.filter (either (const False) (`elem` ac) . fst) $ (\txo -> (scriptToAddressBS . HK.scriptOutput $ txo,txo)) <$> HK.txOut btx

calculateOutputStatus :: (Bool, Bool) -> TransOutputType
calculateOutputStatus (isSpent, isOurs) = case (isSpent, isOurs) of
  (True, True) -> TOSpent
  (False, True) -> TOUnspent
  _ -> TOUnknown

-- | Calculates statuses (Spent / Unspent / Unknown) of transaction outputs based on stored transactions and addresses.
-- Statuses of outputs in resulting list are in the same order as outputs in transaction.
getOutsStatuses :: (MonadIO m, PlatformNatives) => [BtcTx] -> [BtcAddress] -> BtcTx -> m [TransOutputType]
getOutsStatuses storedTxs storedAddrs tx = do
  isOursOutCheckResults <- traverse (checkOutIsOursBtc storedAddrs) outsToCheck
  let outsStatuses = calculateOutputStatus <$> L.zip spentCheckResults isOursOutCheckResults
  pure outsStatuses
  where
    storedBtcTxs = getBtcTx <$> storedTxs
    tx' = getBtcTx tx
    txHash = HK.txHash tx'
    outsToCheck = HK.txOut tx'
    outsCount = L.length outsToCheck
    outPointsToCheck = uncurry HK.OutPoint <$> L.zip (L.repeat txHash) [0..(fromIntegral outsCount - 1)]
    inputsOfStoredTxs = HK.txIn <$> storedBtcTxs
    spentCheckResults = checkOutSpent inputsOfStoredTxs <$> outPointsToCheck
