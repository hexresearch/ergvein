module Ergvein.Wallet.Transaction.Get(
    transactionsGetting
  ) where

import Control.Monad.Reader
import Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Word
import Network.Haskoin.Address

import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings
import Ergvein.Wallet.TimeZone
import Ergvein.Wallet.Transaction.Util
import Ergvein.Wallet.Transaction.View

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Network.Haskoin.Block          as HK
import qualified Network.Haskoin.Transaction    as HK

txListRaw ::
  [Maybe HK.BlockHeader] ->
  [Maybe HK.BlockHash] ->
  [EgvTx] ->
  [Money] ->
  [Bool] ->
  [[Maybe EgvTx]] ->
  [[TransOutputType]] ->
  [[TxId]] ->
  [TxRawInfo]
txListRaw (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) = (TxRawInfo a b c d e f g h) : txListRaw as bs cs ds es fs gs hs
txListRaw _ _ _ _ _ _ _ _ = []

transactionsGetting :: MonadFront t m => Currency -> m (Dynamic t [TransactionView], Dynamic t Word64)
transactionsGetting cur = do
  buildE <- delay 0.2 =<< getPostBuild
  settings <- getSettings
  pubStorageD <- getPubStorageD
  let getHeight pubStorage' = fromMaybe 0 $ _currencyPubStorage'height =<< Map.lookup cur (_pubStorage'currencyPubStorages pubStorage')
      heightD = getHeight <$> pubStorageD
      allBtcAddrsD = ffor pubStorageD $ \PubStorage{..} -> case Map.lookup BTC _pubStorage'currencyPubStorages of
        Nothing -> []
        Just CurrencyPubStorage{..} -> V.toList $ extractAddrs _currencyPubStorage'pubKeystore
  timeZoneE <- getGetTimeZone buildE
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
    let txHashes = fmap (HK.txHash . getBtcTx) txs
        txsRefList = fmap ((calcRefill cur) (fmap getBtcAddr allBtcAddrs)) txs
        parentTxsIds = (fmap . fmap) (hkTxHashToEgv . HK.outPointHash . HK.prevOutput) (fmap (HK.txIn . getBtcTx) txs)
    blh <- traverse getBtcBlockHashByTxHash txHashes
    bl <- traverse (maybe (pure Nothing) getBlockHeaderByHash) blh
    txStore <- getTxStorage cur
    flip runReaderT txStore $ do
      bInOut <- traverse (checkAddrInOut allBtcAddrs) txs
      parentTxs <- sequenceA $ fmap (traverse getTxById) parentTxsIds
      txStore' <- ask
      let conflictingTxs = getConflictingTxs txs -- This might be inefficient, better to calculate this only for unconfirmed txs
          storedTxs = Map.elems txStore'
          getTxConfirmations mTx = case mTx of
            Nothing -> 1 -- If tx is not found in our storage we prefer to treat it as confirmed
            Just tx -> maybe 0 (countConfirmations hght) (fmap etxMetaHeight $ getBtcTxMeta tx)
          txParentsConfirmations = (fmap . fmap) getTxConfirmations parentTxs
          hasUnconfirmedParents = fmap (L.any (== 0)) txParentsConfirmations -- This might be inefficient, better to calculate this only for unconfirmed txs
      outsStatuses <- traverse (getOutsStatuses storedTxs allBtcAddrs) txs
      let rawTxsL = L.filter (\(a,_) -> a/=Nothing) $ L.zip bInOut $ txListRaw bl blh txs txsRefList hasUnconfirmedParents parentTxs outsStatuses conflictingTxs
          prepTxs = L.sortOn txDate $ (prepareTransactionView allBtcAddrs hght timeZone (maybe btcDefaultExplorerUrls id $ Map.lookup cur (settingsExplorerUrl settings)) <$> rawTxsL)
      pure $ L.reverse $ addWalletState prepTxs

filterTx :: Currency -> p -> PubStorage -> [EgvTx]
filterTx cur _ pubS = case cur of
  BTC  -> fmap snd $ fromMaybe [] $ fmap Map.toList $ _currencyPubStorage'transactions <$> Map.lookup cur (_pubStorage'currencyPubStorages pubS)
  ERGO -> []

calcRefill :: Foldable t => Currency -> t Address -> EgvTx -> Money
calcRefill cur ac tx = case tx of
    BtcTx btx _ -> Money cur $ sum $ fmap (HK.outValue . snd) $ L.filter (either (const False) (flip elem ac) . fst) $ fmap (\txo -> (scriptToAddressBS . HK.scriptOutput $ txo,txo)) $ HK.txOut btx
    ErgTx _ _ -> Money cur 0

calculateOutputStatus :: (Bool, Bool) -> TransOutputType
calculateOutputStatus (isSpent, isOurs) = case (isSpent, isOurs) of
  (True, True) -> TOSpent
  (False, True) -> TOUnspent
  _ -> TOUnknown

-- | Calculates statuses (Spent / Unspent / Unknown) of transaction outputs based on stored transactions and addresses.
-- Statuses of outputs in resulting list are in the same order as outputs in transaction.
getOutsStatuses :: (MonadIO m, PlatformNatives) => [EgvTx] -> [EgvAddress] -> EgvTx -> m [TransOutputType]
getOutsStatuses storedTxs storedAddrs tx = do
  isOursOutCheckResults <- traverse (checkOutIsOurs storedAddrs) outsToCheck
  let outsStatuses = calculateOutputStatus <$> L.zip spentCheckResults isOursOutCheckResults
  pure outsStatuses
  where
    storedTxs' = getBtcTx <$> storedTxs
    tx' = getBtcTx tx
    txHash = HK.txHash tx'
    outsToCheck = HK.txOut tx'
    outsCount = L.length outsToCheck
    outPointsToCheck = (uncurry HK.OutPoint) <$> L.zip (L.repeat txHash) [0..(fromIntegral outsCount - 1)]
    inputsOfStoredTxs = HK.txIn <$> storedTxs'
    spentCheckResults = (checkOutSpent inputsOfStoredTxs) <$> outPointsToCheck
