module Ergvein.Wallet.Scan (
    accountDiscovery
  ) where

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
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

import qualified Data.IntMap.Strict                 as MI
import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as T
import qualified Ergvein.Wallet.Filters.Scan        as Filters
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT
import qualified Network.Haskoin.Transaction.Segwit as HTS
import qualified Network.Haskoin.Address            as HA

-- | Loads current PubStorage, performs BIP44 account discovery algorithm and
-- stores updated PubStorage to the wallet file.
accountDiscovery :: MonadFront t m => m ()
accountDiscovery = do
  logWrite "Account discovery started"
  pubStorages <- _pubStorage'currencyPubStorages <$> getPubStorage
  updatedPubStoragesE <- scan pubStorages
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
scan :: MonadFront t m => CurrencyPubStorages -> m (Event t CurrencyPubStorages)
scan currencyPubStorages = do
  scanEvents <- traverse (applyScan currencyPubStorages) allCurrencies
  let scanEvents' = [(M.fromList . (: []) <$> e) | e <- scanEvents]
      scanEvents'' = mergeWith M.union scanEvents'
  scannedCurrencyPubStoragesD <- foldDyn M.union M.empty scanEvents''
  let allFinishedE = flip push (updated scannedCurrencyPubStoragesD) $ \updatedCurrencyPubStorage -> do
        pure $ if M.size updatedCurrencyPubStorage == length allCurrencies then Just $ updatedCurrencyPubStorage else Nothing
  pure allFinishedE

-- | Applies scan for a certain currency then returns event with result.
applyScan :: MonadFront t m => CurrencyPubStorages -> Currency -> m (Event t (Currency, CurrencyPubStorage))
applyScan currencyPubStorages currency =
  case M.lookup currency currencyPubStorages of
    Nothing -> fail $ "Could not find currency: " ++ (T.unpack $ currencyName currency)
    Just currencyPubStorage -> scanCurrency currency currencyPubStorage

scanCurrency :: MonadFront t m => Currency -> CurrencyPubStorage -> m (Event t (Currency, CurrencyPubStorage))
scanCurrency currency currencyPubStorage = mdo
  buildE <- getPostBuild
  processKeyE <- traceEvent "Scanning key" <$> (waitFilters currency =<< delay 0 (leftmost [nextKeyE, (0, fstKey) <$ buildE]))
  gapD <- holdDyn 0 gapE
  currentKeyD <- holdDyn (0, fstKey) nextKeyE
  postSync currency $ flip pushAlways gapE $ \cgap -> do
    (ai, _) <- sample . current $ currentKeyD
    pure (ai+1, cgap)
  newKeystoreD <- foldDyn (addXPubKeyToKeystore External) emptyPubKeystore processKeyE
  filterAddressE <- traceEvent "Scanned for blocks" <$> (filterAddress $ (egvXPubKeyToEgvAddress . snd) <$> processKeyE)
  getBlocksE <- traceEvent "Blocks requested" <$> requestBTCBlocks filterAddressE
  storedBlocksE <- storeMultipleBlocksByE getBlocksE
  storedTxHashesE <- storeMultipleBlocksTxHashesByE $ tagPromptlyDyn blocksD storedBlocksE
  let
    noBlocksE = fforMaybe filterAddressE $ \case
      [] -> Just []
      _ -> Nothing
    blocksE = leftmost [getBlocksE, noBlocksE]
  blocksD <- holdDyn [] blocksE
  getTxsE  <- getTxs $ attachPromptlyDynWith (\(_, key) blocks ->
    (egvXPubKeyToEgvAddress key, blocks)) currentKeyD $ tagPromptlyDyn blocksD storedTxHashesE
  newTxsD <- foldDyn M.union emptyTxs getTxsE
  let txsE = leftmost [getTxsE, mempty <$ noBlocksE]
  let fstKey = derivePubKey masterPubKey External (fromIntegral 0)
      pubKeystore = _currencyPubStorage'pubKeystore currencyPubStorage
      masterPubKey = pubKeystore'master pubKeystore
      emptyPubKeystore = PubKeystore masterPubKey MI.empty MI.empty
      emptyTxs = M.empty
      gapE = flip pushAlways txsE $ \txs -> do
        gap <- sampleDyn gapD
        pure $ if null txs && gap < gapLimit then gap + 1 else gapLimit
      nextKeyE = flip push gapE $ \gap -> do
        gap <- sampleDyn gapD
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
