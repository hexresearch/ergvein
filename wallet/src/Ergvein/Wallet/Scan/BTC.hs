module Ergvein.Wallet.Scan.BTC (
    scanBTC
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Dependent.Map (DMap, DSum((:=>)))
import Data.List
import Data.Map.Strict (Map)
import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Blocks.BTC
import Ergvein.Wallet.Blocks.Storage
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Log.Event
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Tx.BTC
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map                 as DM
import qualified Data.IntMap.Strict                 as MI
import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as T
import qualified Ergvein.Wallet.Filters.Scan        as Filters
import qualified Network.Haskoin.Address            as HA
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT

-- | Scan external addresses first, then internal addresses.
scanBTC :: MonadFront t m => CurrencyPubStorage BtcTx -> m (Event t (CurrencyPubStorage BtcTx))
scanBTC currencyPubStorage = do
  -- Updates external keys and transactions in CurrencyPubStorage
  externalKeysE <- scanExternalAddresses currencyPubStorage
  -- Updates internal keys in CurrencyPubStorage
  internalKeysE <- scanInternalAddressesByE externalKeysE
  pure internalKeysE

scanExternalAddresses :: MonadFront t m => CurrencyPubStorage BtcTx -> m (Event t (CurrencyPubStorage BtcTx))
scanExternalAddresses currencyPubStorage = mdo
  let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
      masterPubKey = pubKeystore'master pubKeystore
      emptyPubKeyStore = PubKeystore masterPubKey MI.empty MI.empty
      startGap = 0
      startKeyIndex = 0
      startKey = derivePubKey masterPubKey External (fromIntegral $ startKeyIndex)
      gapE = flip pushAlways getTxsE $ \txs -> do
        gap <- sampleDyn gapD
        pure $ if null txs && gap < gapLimit then gap + 1 else startGap
      nextKeyE = flip push gapE $ \gap -> do
        currentKeyIndex <- fmap fst (sampleDyn currentKeyD)
        let nextKeyIndex = currentKeyIndex + 1
        pure $ if gap >= gapLimit
          then Nothing
          else Just $ (nextKeyIndex, derivePubKey masterPubKey External (fromIntegral nextKeyIndex))
      finishedE = flip push gapE $ \gap -> do
        newKeystore <- sampleDyn newKeystoreD
        newTxs <- sampleDyn newTxsD
        pure $ if gap >= gapLimit then Just $ CurrencyPubStorage newKeystore newTxs else Nothing
  gapD <- holdDyn startGap gapE
  currentKeyD <- holdDyn (startKeyIndex, startKey) nextKeyE
  newKeystoreD <- foldDyn (addXPubKeyToKeystore External) emptyPubKeyStore processKeyE
  newTxsD <- foldDyn M.union M.empty getTxsE
  buildE <- getPostBuild
  processKeyE <- logEventWith (\(keyIndex, key) ->
    "Scanning external BTC address #" <> showt keyIndex <>
    ": \"" <> (egvAddrToString $ egvXPubKeyToEgvAddress key) <> "\"")
    =<< waitFilters (leftmost [nextKeyE, (startKeyIndex, startKey) <$ buildE])
  postSync $ flip pushAlways gapE $ \currnetGap -> do
    (keyIndex, _) <- sample . current $ currentKeyD
    pure (keyIndex + 1, currnetGap)
  getTxsE <- scanExternalAddress processKeyE currentKeyD
  pure finishedE

scanExternalAddress :: MonadFront t m => Event t (Int, EgvXPubKey) -> Dynamic t (Int, EgvXPubKey) -> m (Event t (Map TxId BtcTx))
scanExternalAddress processKeyE currentKeyD = mdo
  let noBlocksE = fforMaybe filterAddressE $ \case
        [] -> Just []
        _ -> Nothing
      blocksE = leftmost [getBlocksE, noBlocksE]
      txsE = leftmost [getTxsE, mempty <$ noBlocksE]
  blocksD <- holdDyn [] blocksE
  filterAddressE <- logEvent "Scanned for blocks: " =<< (filterAddress $ (egvXPubKeyToEgvAddress . snd) <$> processKeyE)
  getBlocksE <- logEvent "Blocks requested: " =<< requestBTCBlocks filterAddressE
  storedBlocksE <- storeMultipleBlocksByE getBlocksE
  storedTxHashesE <- storeMultipleBlocksTxHashesByE $ tagPromptlyDyn blocksD storedBlocksE
  getTxsE <- getTxs $ attachPromptlyDynWith (\(_, key) blocks ->
    (egvXPubKeyToEgvAddress key, blocks)) currentKeyD $ tagPromptlyDyn blocksD storedTxHashesE
  pure getTxsE

type CurrentGap = Int
type AddressNum = Int

-- | Show to user how far we are went in syncing addresses
postSync :: MonadFront t m => Event t (AddressNum, CurrentGap) -> m ()
postSync e = setSyncProgress $ ffor e $ \(ai, gnum) -> if gnum >= gapLimit
  then Synced
  else SyncMeta BTC (SyncAddress ai) (fromIntegral gnum) (fromIntegral gapLimit)

scanInternalAddressesByE :: MonadFront t m => Event t (CurrencyPubStorage BtcTx) -> m (Event t (CurrencyPubStorage BtcTx))
scanInternalAddressesByE = performEvent . fmap (scanInternalAddresses startKeyIndex startGap)
  where startKeyIndex = 0
        startGap = 0

-- scanInternalAddresses :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => CurrencyPubStorage tx -> m (CurrencyPubStorage tx)
-- scanInternalAddresses currencyPubStorage = do
--   let startKeyIndex = 0
--   newKeystore <- scanInternalAddressesLoop currencyPubStorage startKeyIndex 0
--   pure $ BTCTag :=> newKeystore

scanInternalAddresses :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => Int -> Int -> CurrencyPubStorage BtcTx -> m (CurrencyPubStorage BtcTx)
scanInternalAddresses keyIndex gap currencyPubStorage
  | gap == gapLimit = pure currencyPubStorage
  | otherwise = do
    let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
        masterPubKey = pubKeystore'master pubKeystore
        key = derivePubKey masterPubKey Internal (fromIntegral keyIndex)
        address = egvXPubKeyToEgvAddress key
        txs = currencyPubStorage ^. currencyPubStorage'transactions
        updatedPubKeystore = addXPubKeyToKeystore Internal (keyIndex, key) pubKeystore
        updatedPubStorage = currencyPubStorage & currencyPubStorage'pubKeystore .~ updatedPubKeystore
    logWrite $ "Scanning internal BTC address #" <> showt keyIndex <> ": \"" <> egvAddrToString address <> "\""
    checkResults <- traverse (checkAddrTx address) txs
    if (M.size $ M.filter id checkResults) > 0
      then scanInternalAddresses (keyIndex + 1) 0 updatedPubStorage
      else scanInternalAddresses (keyIndex + 1) (gap + 1) updatedPubStorage

-- | If the given event fires and there is not fully synced filters. Wait for the synced filters and then fire the event.
waitFilters :: MonadFront t m => Event t a -> m (Event t a)
waitFilters e = mdo
  eD <- holdDyn Nothing $ leftmost [Just <$> e, Nothing <$ passValE']
  syncedD <- getFiltersSync BTC
  let passValE = fmapMaybe id $ updated $ foo <$> eD <*> syncedD
      notSyncE = attachWithMaybe
        (\b _ -> if b then Nothing else Just "Waiting filters sync...") (current syncedD) e
  performEvent_ $ logWrite <$> notSyncE
  passValE' <- delay 0.01 passValE
  pure passValE
  where
    foo :: Maybe a -> Bool -> Maybe a
    foo ma b = case (ma,b) of
      (Just a, True) -> Just a
      _ -> Nothing

-- | Gets a list of block hashes containing transactions related to given address.
filterAddress :: MonadFront t m => Event t EgvAddress -> m (Event t [HB.BlockHash])
filterAddress addrE = performFork $ ffor addrE Filters.filterAddress

-- | Gets transactions related to given address from given block list.
getTxs :: MonadFront t m => Event t (EgvAddress, [HB.Block]) -> m (Event t (Map TxId BtcTx))
getTxs = performEvent . fmap (uncurry getAddrTxsFromBlocks)

-- | Gets transactions related to given address from given block.
getAddrTxsFromBlocks :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> [HB.Block] -> m (Map TxId BtcTx)
getAddrTxsFromBlocks addr blocks = do
  txMaps <- traverse (getAddrTxsFromBlock addr) blocks
  pure $ M.unions txMaps

getAddrTxsFromBlock :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HB.Block -> m (Map TxId BtcTx)
getAddrTxsFromBlock addr block = do
  checkResults <- traverse (checkAddrTx addr) txs
  let filteredTxs = fst $ unzip $ filter snd (zip txs checkResults)
  pure $ M.fromList [(HT.txHashToHex $ HT.txHash tx, tx) | tx <- filteredTxs]
  where txs = HB.blockTxns block
