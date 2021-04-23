{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Get(
    transactionsGetting
) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe (fromMaybe, fromJust)
import Data.Time
import Data.Word
import Network.Haskoin.Address
import Reflex.Flunky
import Reflex.Fork
import Reflex.Main.Thread

import Ergvein.Core.Settings
import Ergvein.Core.TimeZone
import Ergvein.Core.Transaction.Util
import Ergvein.Core.Transaction.View
import Ergvein.Core.Transaction.Btc
import Ergvein.Core.Wallet.Monad
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Transaction
import Sepulcas.Native

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Network.Haskoin.Block          as HK
import qualified Network.Haskoin.Transaction    as HK

txListRaw ::
  [Maybe HK.BlockHeader] ->
  [Maybe HK.BlockHash] ->
  [EgvTx] ->
  [Money] ->
  [Bool] ->
  [[TransOutputType]] ->
  [[Maybe HK.TxOut]] ->
  [[TxId]] ->
  [[TxId]] ->
  [(Bool, [TxId])] ->
  [TxRawInfo]
txListRaw (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is) (j:js) = (TxRawInfo a b c d e f g h i j) : txListRaw as bs cs ds es fs gs hs is js
txListRaw _ _ _ _ _ _ _ _ _ _ = []

transactionsGetting :: (MonadWallet t m, MonadSettings t m, PerformMain t m) => Currency -> m (Dynamic t [TransactionView], Dynamic t Word64)
transactionsGetting cur = do
  buildE <- delay 0.2 =<< getPostBuild
  settings <- getSettings
  pubStorageD <- getPubStorageD
  let getHeight pubStorage' = maybe 0 _currencyPubStorage'chainHeight $ M.lookup cur (_pubStorage'currencyPubStorages pubStorage')
      heightD = getHeight <$> pubStorageD
      allBtcAddrsD = ffor pubStorageD $ \PubStorage{..} -> case M.lookup BTC _pubStorage'currencyPubStorages of
        Nothing -> []
        Just CurrencyPubStorage{..} -> V.toList $ extractAddrs _currencyPubStorage'pubKeystore
  timeZoneE <- performTimeZone buildE
  timeZoneD <- holdDyn utc timeZoneE
  let txListD = ffor2 allBtcAddrsD pubStorageD (filterTx cur)
      filterE = leftmost [tagPromptlyDyn txListD timeZoneE, updated txListD]
  filteredTxListE <- performFork $ ffor filterE $ \txs -> do
    timeZone <- sampleDyn timeZoneD
    pubStorage' <- sampleDyn pubStorageD
    getAndFilterBlocks cur heightD allBtcAddrsD timeZone txs pubStorage' settings
  filteredTxListD <- holdDyn [] filteredTxListE
  pure (filteredTxListD, heightD)

getAndFilterBlocks :: (HasPubStorage (ReaderT r IO), MonadIO m, Reflex t, PlatformNatives, MonadSample t m) =>
  Currency ->
  Dynamic t BlockHeight ->
  Dynamic t [EgvAddress] ->
  TimeZone ->
  [EgvTx] ->
  r ->
  Settings ->
  m [TransactionView]
getAndFilterBlocks cur heightD btcAddrsD timeZone txs store settings = do
  allBtcAddrs <- sampleDyn btcAddrsD
  hght <- sampleDyn heightD
  liftIO $ flip runReaderT store $ do
    let txHashes = fmap (HK.txHash . getBtcTx . fromJust . toTxBtc) txs
        txsRefList = fmap ((calcRefill cur) (fmap getBtcAddr allBtcAddrs)) txs
        parentTxsIds = (fmap . fmap) (hkTxHashToEgv . HK.outPointHash . HK.prevOutput) (fmap (HK.txIn . getBtcTx . fromJust . toTxBtc) txs)
    blh <- traverse getBtcBlockHashByTxHash txHashes
    bl <- traverse (maybe (pure Nothing) getBlockHeaderByHash) blh
    pubStorage <- askPubStorage
    txStore <- getTxStorage cur
    flip runReaderT txStore $ do
      bInOut <- traverse (checkAddrInOut allBtcAddrs) txs
      parentTxs <- sequenceA $ fmap (traverse getTxById) parentTxsIds
      txStorage <- askTxStorage
      let storedTxs = M.elems txStorage
      outsStatuses <- traverse (getOutsStatuses storedTxs allBtcAddrs) txs
      spentOuts <- traverse (\egvTx -> getOutputsByOutPoints $ HK.prevOutput <$> (HK.txIn . getBtcTx . fromJust . toTxBtc $ egvTx)) txs
      let btcPubStorageMeta = fromMaybe (error "getAndFilterBlocks: BTC storage does not exist!") $ pubStorage ^? pubStorage'currencyPubStorages . at BTC . _Just . currencyPubStorage'meta . _PubStorageBtc
          replacedTxsStore = btcPubStorageMeta ^. btcPubStorage'replacedTxs
          possiblyReplacedTxsStore = btcPubStorageMeta ^. btcPubStorage'possiblyReplacedTxs
          replacedTxs = (fmap . fmap) BtcTxHash $ getReplacedTxs replacedTxsStore txs
          possiblyReplacedTxs = getPossiblyReplacedTxs possiblyReplacedTxsStore txs
          conflictingTxs = (fmap . fmap) BtcTxHash $ getConflictingTxs possiblyReplacedTxs txs -- This might be inefficient, better to calculate this only for unconfirmed txs
          getTxConfirmations mTx = case mTx of
            Nothing -> 1 -- If tx is not found in our storage we prefer to treat it as confirmed
            Just tx -> maybe 0 (countConfirmations hght) (fmap etxMetaHeight $ getBtcTxMeta $ fromJust . toTxBtc $ tx)
          txParentsConfirmations = (fmap . fmap) getTxConfirmations parentTxs
          hasUnconfirmedParents = fmap (L.any (== 0)) txParentsConfirmations -- This might be inefficient, better to calculate this only for unconfirmed txs
          explorerUrls = btcSettings'explorerUrls $ getBtcSettings settings
          rawTxsL = L.filter (\(a,_) -> a /= Nothing) $ L.zip bInOut $ txListRaw bl blh txs txsRefList hasUnconfirmedParents outsStatuses spentOuts conflictingTxs replacedTxs ((fmap . fmap . fmap) BtcTxHash possiblyReplacedTxs)
          prepTxs = L.sortOn txDate $ (prepareTransactionView allBtcAddrs hght timeZone explorerUrls <$> rawTxsL)
      pure $ L.reverse $ addWalletState prepTxs

filterTx :: Currency -> p -> PubStorage -> [EgvTx]
filterTx cur _ pubS = case cur of
  BTC  -> fmap snd $ fromMaybe [] $ fmap M.toList $ _currencyPubStorage'transactions <$> M.lookup cur (_pubStorage'currencyPubStorages pubS)
  ERGO -> []

calcRefill :: Foldable t => Currency -> t Address -> EgvTx -> Money
calcRefill cur ac tx = case tx of
  TxBtc (BtcTx btx _) -> Money cur $ sum $ fmap (HK.outValue . snd) $ L.filter (either (const False) (flip elem ac) . fst) $ fmap (\txo -> (scriptToAddressBS . HK.scriptOutput $ txo,txo)) $ HK.txOut btx
  TxErg (ErgTx _ _) -> Money cur 0

calculateOutputStatus :: (Bool, Bool) -> TransOutputType
calculateOutputStatus (isSpent, isOurs) = case (isSpent, isOurs) of
  (True, True) -> TOSpent
  (False, True) -> TOUnspent
  _ -> TOUnknown

-- | Calculates statuses (Spent / Unspent / Unknown) of transaction outputs based on stored transactions and addresses.
-- Statuses of outputs in resulting list are in the same order as outputs in transaction.
getOutsStatuses :: (MonadIO m, PlatformNatives) => [EgvTx] -> [EgvAddress] -> EgvTx -> m [TransOutputType]
getOutsStatuses storedTxs storedAddrs tx = do
  isOursOutCheckResults <- traverse (checkOutIsOursBtc storedBtcAddrs) outsToCheck
  let outsStatuses = calculateOutputStatus <$> L.zip spentCheckResults isOursOutCheckResults
  pure outsStatuses
  where
    storedBtcTxs = getBtcTx . fromJust . toTxBtc <$> storedTxs
    storedBtcAddrs = mapMaybe (\addr -> case addr of (BtcAddress addr') -> Just addr'; _ -> Nothing) storedAddrs
    tx' = getBtcTx . fromJust . toTxBtc $ tx
    txHash = HK.txHash tx'
    outsToCheck = HK.txOut tx'
    outsCount = L.length outsToCheck
    outPointsToCheck = (uncurry HK.OutPoint) <$> L.zip (L.repeat txHash) [0..(fromIntegral outsCount - 1)]
    inputsOfStoredTxs = HK.txIn <$> storedBtcTxs
    spentCheckResults = (checkOutSpent inputsOfStoredTxs) <$> outPointsToCheck
