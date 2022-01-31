{-# OPTIONS_GHC -Wall #-}
module Ergvein.Core.Store.Rbf(
    RemoveRbfTxsInfo(..)
  , removeTxsReplacedByUnconfirmedTx
  , removeTxsReplacedByConfirmedTx
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map (Map)
import Data.Text (Text)
import Data.Maybe
import Data.Set (Set)
import Ergvein.Core.Transaction.Btc
import Ergvein.Core.Wallet
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Btc
import Reflex.Fork

import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Network.Haskoin.Transaction        as HT
import qualified Data.List as L

-- | Finds all txs that should be replaced by given unconfirmed tx and removes them from storage.
-- Also stores information about transaction replacements in the storage.
-- Information about which transactions were replaced is stored in currencyPubStorage'replacedTxs.
-- Since we are not always able to identify the transactions with the highest fee in a sequence of RBF transactions,
-- we also keep information about which of these transactions should be deleted when one of them is confirmed.
-- This information is stored in currencyPubStorage'possiblyReplacedTxs.
removeTxsReplacedByUnconfirmedTx :: MonadStorage t m =>
  Text ->
  Event t BtcTxRaw ->
  m (Event t BtcTxRaw)
removeTxsReplacedByUnconfirmedTx caller replacingTxE = do
  replacingTxD <- holdDyn Nothing (Just <$> replacingTxE)
  pubStorageD <- getPubStorageD
  replacedTxsE <- performFork $ ffor (current pubStorageD `attach` replacingTxE) $ \(ps, replacingTx) -> do
    let btcps = ps ^. btcPubStorage
        txStore = btcps ^. currencyPubStorage'transactions
        possiblyReplacedTxStore = fromMaybe M.empty $ btcps ^? currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'possiblyReplacedTxs
    liftIO $ flip runReaderT txStore $ do
      unconfirmedTxs <- getUnconfirmedTxs
      let unconfirmedBtcTxs = getBtcTx . fromJust . toTxBtc <$> unconfirmedTxs
          replacingBtcTxId = HT.txHash replacingTx
          replacingTxId = BtcTxHash replacingBtcTxId
          otherUnconfirmedTxs = M.elems $ M.delete replacingTxId unconfirmedBtcTxs
      directlyReplacedTxs <- filterM (fmap (== Just True) . replacesByFee replacingTx) otherUnconfirmedTxs
      directlyReplacedTxsChilds <- L.concat <$> traverse getChildTxs directlyReplacedTxs
      possiblyReplacedTxs <- filterM (fmap (== Nothing) . replacesByFee replacingTx) otherUnconfirmedTxs
      possiblyReplacedTxsChilds <- L.concat <$> traverse getChildTxs possiblyReplacedTxs
      -- Also remove all txs that are in the same group in btcPubStorage'possiblyReplacedTxs
      -- with replaced txs.
      let possiblyReplacedTxsGroups = uncurry S.insert <$> M.toList possiblyReplacedTxStore

          -- Gets txs that should be replaced form btcPubStorage'possiblyReplacedTxs if provided tx has been replaced
          getTxsFromGroups :: [S.Set BtcTxId] -> BtcTxId -> S.Set BtcTxId
          getTxsFromGroups txGroups tx = S.unions $ (\txGroup -> if S.member tx txGroup then S.delete tx txGroup else S.empty) <$> txGroups

          indirectlyReplacedTxs = S.toList $ S.unions $ getTxsFromGroups possiblyReplacedTxsGroups <$> (HT.txHash <$> directlyReplacedTxs)
          replacedTxIds = S.fromList $ (HT.txHash <$> (directlyReplacedTxs ++ directlyReplacedTxsChilds)) ++ indirectlyReplacedTxs
          possiblyReplacedTxIds = S.fromList $ HT.txHash <$> (possiblyReplacedTxs ++ possiblyReplacedTxsChilds)
      pure (replacingBtcTxId, replacedTxIds, possiblyReplacedTxIds)
  removedE <- modifyPubStorage clr $ removeRbfTxsFromStorage1 <$> replacedTxsE
  pure $ fmapMaybe id (tagPromptlyDyn replacingTxD removedE)
  where clr = caller <> ":" <> "removeTxsReplacedByFee"

removeRbfTxsFromStorage1 :: (BtcTxId, Set BtcTxId, Set BtcTxId) -> PubStorage -> Maybe PubStorage
removeRbfTxsFromStorage1 (replacingTxId, replacedTxIds, possiblyReplacedTxIds) ps =
  if L.null replacedTxIds && L.null possiblyReplacedTxIds
    then Nothing
    else Just $ let
      -- Removing txs from btcPubStorage'transactions
      ps11 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'transactions %~ (`M.withoutKeys` replacedTxIds)) ps
      -- Removing utxos from btcPubStorage'utxos
      ps12 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'utxos %~ M.filterWithKey (filterOutPoints replacedTxIds)) ps11
      -- Removing txids from EgvPubKeyBoxes in currencyPubStorage'pubKeystore
      ps13 = modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'pubKeystore %~ removeTxIdsFromEgvKeyBoxes (S.map BtcTxHash replacedTxIds)) ps12
      -- Updating btcPubStorage'replacedTxs
      ps14 = modifyCurrStorageBtc (updateReplacedTxsStorage replacingTxId replacedTxIds) ps13
      -- Updating btcPubStorage'possiblyReplacedTxs
      ps15 = modifyCurrStorageBtc (updatePossiblyReplacedTxsStorage replacingTxId replacedTxIds possiblyReplacedTxIds) ps14
      in ps15

updateReplacedTxsStorage :: BtcTxId -> Set BtcTxId -> BtcPubStorage -> BtcPubStorage
updateReplacedTxsStorage replacingTxId replacedTxIds btcPs =
  let
    replacedTxsMap = btcPs ^. btcPubStorage'replacedTxs
    -- Remove all replacedTxsMap keys that are members of replacedTxIds
    replacedTxsMap' = M.filterWithKey (\k _ -> not $ k `S.member` replacedTxIds) replacedTxsMap
    -- Collect values of removed replacedTxsMap keys into one set
    txIdsReplacedByFilteredTxIds = S.unions $ S.map (flip (M.findWithDefault S.empty) replacedTxsMap) replacedTxIds
    -- Combine into one set
    updatedReplacedTxIds = S.unions [replacedTxIds, txIdsReplacedByFilteredTxIds]
    -- Insert replacing tx with replaced txs into map
    updatedReplacedTxs = M.insertWith S.union replacingTxId updatedReplacedTxIds replacedTxsMap'
    updatedBtcPs = btcPs & btcPubStorage'replacedTxs .~ updatedReplacedTxs
  in updatedBtcPs

updatePossiblyReplacedTxsStorage :: BtcTxId -> Set BtcTxId -> Set BtcTxId -> BtcPubStorage -> BtcPubStorage
updatePossiblyReplacedTxsStorage possiblyReplacingTxId replacedTxIds possiblyReplacedTxIds btcPs =
  let
    possiblyReplacedTxsMap = btcPs ^. btcPubStorage'possiblyReplacedTxs
    -- Remove all keys that are members of possiblyReplacedTxIds
    possiblyReplacedTxsMap' = M.filterWithKey (\k _ -> not $ k `S.member` possiblyReplacedTxIds) possiblyReplacedTxsMap
    -- Collect values of removed keys into one set
    txIdsReplacedByFilteredTxIds = S.unions $ S.map (flip (M.findWithDefault S.empty) possiblyReplacedTxsMap) possiblyReplacedTxIds
    -- Combine resulting set with possiblyReplacedTxIds
    updatedPossiblyReplacedTxIds = S.union possiblyReplacedTxIds txIdsReplacedByFilteredTxIds
    -- Insert possibly replacing tx with possibly replaced txs into map
    possiblyReplacedTxsMap'' = M.insertWith S.union possiblyReplacingTxId updatedPossiblyReplacedTxIds possiblyReplacedTxsMap'
    -- If some txs from btcPubStorage'possiblyReplacedTxs have been replaced by replacingTxId
    -- then we need to delete the corresponding group
    updatedPossiblyReplacedTxs  = M.filterWithKey (\k v -> not (k `S.member` replacedTxIds) && S.null (S.intersection v replacedTxIds)) possiblyReplacedTxsMap''
    updatedBtcPs = btcPs & btcPubStorage'possiblyReplacedTxs .~ updatedPossiblyReplacedTxs
  in updatedBtcPs

data RemoveRbfTxsInfo = RemoveRbfTxsInfo {
    removeRbfTxsInfo'keyToRemoveFromPossiblyReplacedTxs :: !BtcTxId -- ^ Map key that should be removed from currencyPubStorage'possiblyReplacedTxs.
  , removeRbfTxsInfo'replacingTx :: !BtcTxId -- ^ Tx that replaces txs in removeRbfTxsInfo'replacedTxs.
  , removeRbfTxsInfo'replacedTxs :: !(Set BtcTxId) -- ^ Set of txs that was replaced and should be removed from storage.
  } deriving (Eq, Show, Ord)

-- | Finds all txs that should be replaced by given confirmed tx and removes them from storage.
-- Also stores information about transaction replacements in the storage.
removeTxsReplacedByConfirmedTx :: MonadStorage t m => Event t () -> m (Event t ())
removeTxsReplacedByConfirmedTx goE = do
  pubStorageD <- getPubStorageD
  replacedTxsE <- performFork $ ffor (tagPromptlyDyn pubStorageD goE) $ \ps -> do
    let btcps = ps ^. btcPubStorage
        txStore = btcps ^. currencyPubStorage'transactions
        possiblyReplacedTxs = btcps ^. currencyPubStorage'meta . _PubStorageBtc . btcPubStorage'possiblyReplacedTxs
    liftIO $ flip runReaderT txStore $ do
      confirmedTxIds <- M.keysSet <$> getConfirmedTxs
      let confirmedBtcTxIds = S.map (fromJust . toBtcTxHash) confirmedTxIds
      pure $ getTxsToRemove confirmedBtcTxIds possiblyReplacedTxs
  modifyPubStorage "removedReplacedTxs" $ removeRbfTxsFromStorage2 <$> replacedTxsE
  where
  getTxsToRemove ::
    Set BtcTxId ->
    Map BtcTxId (Set BtcTxId) ->
    Set RemoveRbfTxsInfo
  getTxsToRemove confirmedTxIds = M.foldrWithKey' helper S.empty
    where
      helper :: BtcTxId -> Set BtcTxId -> Set RemoveRbfTxsInfo -> Set RemoveRbfTxsInfo
      helper possiblyReplacingTx possiblyReplacedTxs' acc
        | possiblyReplacingTx `S.member` confirmedTxIds = S.insert (RemoveRbfTxsInfo possiblyReplacingTx possiblyReplacingTx possiblyReplacedTxs') acc
        | not $ S.null intersection = S.insert (RemoveRbfTxsInfo possiblyReplacingTx (S.findMin intersection) (S.insert possiblyReplacingTx (S.delete (S.findMin intersection) possiblyReplacedTxs'))) acc
        | otherwise = acc
        where intersection = possiblyReplacedTxs' `S.intersection` confirmedTxIds -- This intersection must contain only one element, because possiblyReplacedTxs are conflicting and no more than one tx may be valid

removeRbfTxsFromStorage2 :: Set RemoveRbfTxsInfo -> PubStorage -> Maybe PubStorage
removeRbfTxsFromStorage2 removeRbfTxsInfoSet ps =
  if S.null removeRbfTxsInfoSet
    then Nothing
    else Just $ let
      keysToRemoveFromPossiblyReplacedTxs = S.map removeRbfTxsInfo'keyToRemoveFromPossiblyReplacedTxs removeRbfTxsInfoSet
      txIdsToRemove = S.unions $ S.map removeRbfTxsInfo'replacedTxs removeRbfTxsInfoSet
      replacedTxsMap = M.fromList $ (\(RemoveRbfTxsInfo _ b c) -> (b, c)) <$> S.toList removeRbfTxsInfoSet
      -- Removing txs from btcPubStorage'transactions
      ps11 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'transactions %~ (`M.withoutKeys` txIdsToRemove)) ps
      -- Removing utxos from btcPubStorage'utxos
      ps12 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'utxos %~ M.filterWithKey (filterOutPoints txIdsToRemove)) ps11
      -- Removing txids from EgvPubKeyBoxes form currencyPubStorage'pubKeystore
      ps13 = modifyCurrStorage BTC (\cps -> cps & currencyPubStorage'pubKeystore %~ removeTxIdsFromEgvKeyBoxes (S.map BtcTxHash txIdsToRemove)) ps12
      -- Updating btcPubStorage'replacedTxs
      ps14 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'replacedTxs %~ M.union replacedTxsMap) ps13
      -- Removing replaced tx ids from btcPubStorage'possiblyReplacedTxs
      ps15 = modifyCurrStorageBtc (\btcPs -> btcPs & btcPubStorage'possiblyReplacedTxs %~ (`M.withoutKeys` keysToRemoveFromPossiblyReplacedTxs)) ps14
      in ps15

filterOutPoints :: Set BtcTxId -> HT.OutPoint -> BtcUtxoMeta -> Bool
filterOutPoints txIdSet outPoint _ = not $ S.member (HT.outPointHash outPoint) txIdSet

removeTxIdsFromEgvKeyBoxes :: Set TxId -> PubKeystore -> PubKeystore
removeTxIdsFromEgvKeyBoxes txIdsSet PubKeystore{..} =
  let updatedPubKeystore'external = fmap removeTxIdsFromKeybox pubKeystore'external
      updatedPubKeystore'internal = fmap removeTxIdsFromKeybox pubKeystore'internal
      removeTxIdsFromKeybox (EgvPubKeyBox k txs m) = EgvPubKeyBox k (S.difference txs txIdsSet) m
  in PubKeystore pubKeystore'master updatedPubKeystore'external updatedPubKeystore'internal
