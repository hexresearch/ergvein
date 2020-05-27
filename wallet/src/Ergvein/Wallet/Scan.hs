module Ergvein.Wallet.Scan (
    accountDiscovery
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.List
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
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Util

import qualified Data.IntMap.Strict                 as MI
import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Ergvein.Wallet.Filters.Scan        as Filters
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT

-- | Loads current PubStorage, performs BIP44 account discovery algorithm and
-- stores updated PubStorage to the wallet file.
accountDiscovery :: MonadFront t m => m ()
accountDiscovery = do
  logWrite "Account discovery started"
  pubStorages <- _pubStorage'currencyPubStorages <$> getPubStorage
  ac <- _pubStorage'activeCurrencies <$> getPubStorage
  updatedPubStoragesE <- scan pubStorages ac
  authD <- getAuthInfo
  let updatedAuthE = traceEventWith (const "Account discovery finished") <$>
        flip pushAlways updatedPubStoragesE $ \updatedPubStorages -> do
          auth <- sampleDyn authD
          pure $ Just $ auth
            & authInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages .~ updatedPubStorages
            & authInfo'isUpdate .~ True
  setAuthInfoE <- setAuthInfo updatedAuthE
  storeWallet setAuthInfoE

-- | Gets old CurrencyPubStorages, performs BIP44 account discovery algorithm for all currencies
-- then returns event with updated PubStorage.
scan :: MonadFront t m => CurrencyPubStorages -> [Currency] -> m (Event t CurrencyPubStorages)
scan currencyPubStorages curs = do
  scanEvents <- traverse (applyScan currencyPubStorages) curs
  let scanEvents' = [(M.fromList . (: []) <$> e) | e <- scanEvents]
      scanEvents'' = mergeWith M.union scanEvents'
  scannedCurrencyPubStoragesD <- foldDyn M.union M.empty scanEvents''
  let finishedE = flip push (updated scannedCurrencyPubStoragesD) $ \updatedCurrencyPubStorage -> do
        pure $ if M.size updatedCurrencyPubStorage == length allCurrencies then Just $ updatedCurrencyPubStorage else Nothing
  pure finishedE

-- | Applies scan for a certain currency then returns event with result.
applyScan :: MonadFront t m => CurrencyPubStorages -> Currency -> m (Event t (Currency, CurrencyPubStorage))
applyScan currencyPubStorages currency =
  case M.lookup currency currencyPubStorages of
    Nothing -> fail $ "Could not find currency: " ++ (T.unpack $ currencyName currency)
    Just currencyPubStorage -> scanCurrency currency currencyPubStorage

-- | Scan external addresses first, then internal addresses.
scanCurrency :: MonadFront t m => Currency -> CurrencyPubStorage -> m (Event t (Currency, CurrencyPubStorage))
scanCurrency currency currencyPubStorage = do
  -- Updates external keys and transactions in CurrencyPubStorage
  externalKeysE <- scanExternalAddresses currency currencyPubStorage
  -- Updates internal keys in CurrencyPubStorage
  internalKeysE <- scanInternalAddressesByE externalKeysE
  pure internalKeysE

-- | TODO: Revoke and minimize keyindex use
-- Or at least make it consistent with pubKeystore's indexes
scanExternalAddresses :: MonadFront t m => Currency -> CurrencyPubStorage -> m (Event t (Currency, CurrencyPubStorage))
scanExternalAddresses currency currencyPubStorage = mdo
  let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
      masterPubKey = pubKeystore'master pubKeystore
      emptyPubKeyStore = PubKeystore masterPubKey V.empty V.empty
      startGap = 0
      startKeyIndex = 0
      startKey = derivePubKey masterPubKey External (fromIntegral $ startKeyIndex)
  buildE <- getPostBuild
  processKeyE <- logEventWith (\(keyIndex, key) ->
    "Scanning external " <> showt currency <> " address #" <> showt keyIndex <>
    ": \"" <> (egvAddrToString $ egvXPubKeyToEgvAddress key) <> "\"")
    =<< waitFilters currency (leftmost [nextKeyE, (startKeyIndex, startKey) <$ buildE])
  gapD <- holdDyn startGap gapE
  currentKeyD <- holdDyn (startKeyIndex, startKey) nextKeyE
  postSync currency $ flip pushAlways gapE $ \currnetGap -> do
    (keyIndex, _) <- sample . current $ currentKeyD
    pure (keyIndex + 1, currnetGap)
  newKeystoreD <- foldDyn (addXPubKeyToKeystore External . snd) emptyPubKeyStore processKeyE
  newTxsD <- foldDyn M.union M.empty getTxsE
  blocksD <- holdDyn [] blocksE
  filterAddressE <- logEvent "Scanned for blocks: " =<< (filterAddress $ (egvXPubKeyToEgvAddress . snd) <$> processKeyE)
  getBlocksE <- logEvent "Blocks requested: " =<< requestBTCBlocks filterAddressE
  storedBlocksE <- storeMultipleBlocksByE getBlocksE
  storedTxHashesE <- storeMultipleBlocksTxHashesByE $ tagPromptlyDyn blocksD storedBlocksE
  getTxsE <- getTxs $ attachPromptlyDynWith (\(_, key) blocks ->
    (egvXPubKeyToEgvAddress key, blocks)) currentKeyD $ tagPromptlyDyn blocksD storedTxHashesE
  let noBlocksE = fforMaybe filterAddressE $ \case
        [] -> Just []
        _ -> Nothing
      blocksE = leftmost [getBlocksE, noBlocksE]
      txsE = leftmost [getTxsE, mempty <$ noBlocksE]
      gapE = flip pushAlways txsE $ \txs -> do
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
        pure $ if gap >= gapLimit then Just $ (currency, CurrencyPubStorage newKeystore newTxs) else Nothing
  pure finishedE

type CurrentGap = Int
type AddressNum = Int

-- | Show to user how far we are went in syncing addresses
postSync :: MonadFront t m => Currency -> Event t (AddressNum, CurrentGap) -> m ()
postSync cur e = setSyncProgress $ ffor e $ \(ai, gnum) -> if gnum >= gapLimit
  then Synced
  else SyncMeta cur (SyncAddress ai) (fromIntegral gnum) (fromIntegral gapLimit)

scanInternalAddressesByE :: MonadFront t m => Event t (Currency, CurrencyPubStorage) -> m (Event t (Currency, CurrencyPubStorage))
scanInternalAddressesByE = performEvent . fmap scanInternalAddresses

scanInternalAddresses :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => (Currency, CurrencyPubStorage) -> m (Currency, CurrencyPubStorage)
scanInternalAddresses (currency, currencyPubStorage) = do
  let startKeyIndex = 0
  newKeystore <- scanInternalAddressesLoop currencyPubStorage startKeyIndex 0
  pure (currency, newKeystore)

scanInternalAddressesLoop :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => CurrencyPubStorage -> Int -> Int -> m CurrencyPubStorage
scanInternalAddressesLoop currencyPubStorage keyIndex gap
  | gap == gapLimit = pure currencyPubStorage
  | otherwise = do
    let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
        masterPubKey = pubKeystore'master pubKeystore
        key = derivePubKey masterPubKey Internal (fromIntegral keyIndex)
        address = egvXPubKeyToEgvAddress key
        txs = currencyPubStorage ^. currencyPubStorage'transactions
        updatedPubKeystore = addXPubKeyToKeystore Internal key pubKeystore
        updatedPubStorage = currencyPubStorage & currencyPubStorage'pubKeystore .~ updatedPubKeystore
    logWrite $ "Scanning internal address #" <> showt keyIndex <> ": \"" <> egvAddrToString address <> "\""
    checkResults <- traverse (checkAddrTx address) $ egvTxsToBtcTxs txs -- TODO: use DMap instead of Map in CurrencyPubStorage
    if (M.size $ M.filter id checkResults) > 0
      then scanInternalAddressesLoop updatedPubStorage (keyIndex + 1) 0
      else scanInternalAddressesLoop updatedPubStorage (keyIndex + 1) (gap + 1)

-- TODO: This function will not be needed after using DMap as a CurrencyPubStorage
egvTxsToBtcTxs :: M.Map TxId EgvTx -> M.Map TxId BtcTx
egvTxsToBtcTxs egvTxMap = M.mapMaybe egvTxToBtcTx egvTxMap
  where egvTxToBtcTx tx = case tx of
          BtcTx t -> Just t
          _ -> Nothing

-- | If the given event fires and there is not fully synced filters. Wait for the synced filters and then fire the event.
waitFilters :: MonadFront t m => Currency -> Event t a -> m (Event t a)
waitFilters c e = mdo
  eD <- holdDyn Nothing $ leftmost [Just <$> e, Nothing <$ passValE']
  syncedD <- getFiltersSync c
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
getTxs :: MonadFront t m => Event t (EgvAddress, [HB.Block]) -> m (Event t (M.Map TxId EgvTx))
getTxs = performEvent . fmap (uncurry getAddrTxsFromBlocks)

-- | Gets transactions related to given address from given block.
getAddrTxsFromBlocks :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> [HB.Block] -> m (M.Map TxId EgvTx)
getAddrTxsFromBlocks addr blocks = do
  txMaps <- traverse (getAddrTxsFromBlock addr) blocks
  pure $ M.unions txMaps

getAddrTxsFromBlock :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HB.Block -> m (M.Map TxId EgvTx)
getAddrTxsFromBlock addr block = do
  checkResults <- traverse (checkAddrTx addr) txs
  let filteredTxs = fst $ unzip $ filter snd (zip txs checkResults)
  pure $ M.fromList [(HT.txHashToHex $ HT.txHash tx, BtcTx tx) | tx <- filteredTxs]
  where txs = HB.blockTxns block
