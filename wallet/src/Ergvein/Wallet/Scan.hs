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
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
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
-- then returns event with updated CurrencyPubStorages.
scan :: MonadFront t m => CurrencyPubStorages -> m (Event t CurrencyPubStorages)
scan currencyPubStorages = do
  scanEvents <- traverse (applyScan currencyPubStorages) allCurrencies
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
  internalKeysE <- scanInternalAddresses externalKeysE
  pure internalKeysE

scanExternalAddresses :: MonadFront t m => Currency -> CurrencyPubStorage -> m (Event t (Currency, CurrencyPubStorage))
scanExternalAddresses currency currencyPubStorage = mdo
  let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
      masterPubKey = pubKeystore'master pubKeystore
      emptyPubKeyStore = PubKeystore masterPubKey MI.empty MI.empty
      startKeyIndex = 0
      startKey = derivePubKey masterPubKey External (fromIntegral $ startKeyIndex)
  buildE <- getPostBuild
  processKeyE <- traceEvent "Scanning key" <$>
    (waitFilters currency =<< delay 0 (leftmost [nextKeyE, (startKeyIndex, startKey) <$ buildE]))
  gapD <- holdDyn 0 gapE
  currentKeyD <- holdDyn (startKeyIndex, startKey) nextKeyE
  newKeystoreD <- foldDyn (addXPubKeyToKeystore External) emptyPubKeyStore processKeyE
  newTxsD <- foldDyn M.union M.empty getTxsE
  filterAddressE <- traceEvent "Filter address key" <$> (filterAddress $ (egvXPubKeyToEgvAddress . snd) <$> processKeyE)
  getBlocksE <- traceEvent "Get blocks" <$> (requestBTCBlocksWaitRN filterAddressE)
  blocksD <- holdDyn [] getBlocksE
  storedBlocksE <- storeMultipleBlocksByE getBlocksE
  storedTxHashesE <- traceEvent "Storing txs" <$> (storeMultipleBlocksTxHashesByE $ tagPromptlyDyn blocksD storedBlocksE)
  getTxsE <- getTxs $ attachPromptlyDynWith (\(_, key) blocks ->
    (egvXPubKeyToEgvAddress key, blocks)) currentKeyD $ tagPromptlyDyn blocksD storedTxHashesE
  let gapE = flip pushAlways getTxsE $ \txs -> do
        gap <- sampleDyn gapD
        pure $ if null txs && gap < gapLimit then gap + 1 else 0
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

scanInternalAddresses :: MonadFront t m => Event t (Currency, CurrencyPubStorage) -> m (Event t (Currency, CurrencyPubStorage))
scanInternalAddresses = performEvent . fmap someFunc

someFunc :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => (Currency, CurrencyPubStorage) -> m (Currency, CurrencyPubStorage)
someFunc (currency, currencyPubStorage) = do
  let startKeyIndex = 0
  newKeystore <- loop currencyPubStorage startKeyIndex 0
  pure (currency, newKeystore)

loop :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => CurrencyPubStorage -> Int -> Int -> m CurrencyPubStorage
loop currencyPubStorage keyIndex gap
  | gap == gapLimit = pure currencyPubStorage
  | otherwise = do
    let pubKeystore = currencyPubStorage ^. currencyPubStorage'pubKeystore
        masterPubKey = pubKeystore'master pubKeystore
        key = derivePubKey masterPubKey Internal (fromIntegral keyIndex)
        address = egvXPubKeyToEgvAddress key
        txs = currencyPubStorage ^. currencyPubStorage'transactions
        updatedPubKeystore = addXPubKeyToKeystore Internal (keyIndex, key) pubKeystore
        updatedPubStorage = currencyPubStorage & currencyPubStorage'pubKeystore .~ updatedPubKeystore
    results <- traverse (checkAddrTx address) $ egvTxsToBtcTxs txs -- TODO: use DMap instead of Map in CurrencyPubStorage
    if (M.size $ M.filter id results) > 0
      then loop updatedPubStorage (keyIndex + 1) 0
      else loop updatedPubStorage (keyIndex + 1) (gap + 1)

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

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HT.Tx -> m Bool
checkAddrTx addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HT.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOut addr) (HT.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = foldr (||) False

-- | Checks given TxIn wheather it contains given address.
-- First, the block containing the spent output is loaded.
-- Then we check the output for the address.
checkTxIn :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HT.TxIn -> m Bool
checkTxIn addr txIn = do
  let spentOutput = HT.prevOutput txIn
      spentTxHash = HT.outPointHash spentOutput
      spentOutputIndex = HT.outPointIndex spentOutput
  mBlockHash <- getBtcBlockHashByTxHash spentTxHash
  case mBlockHash of
    Nothing -> pure False
    Just blockHash -> do
      mBlock <- getBtcBlock blockHash
      case mBlock of
        Nothing -> fail $ "Could not get block from storage by block hash " <> (T.unpack $ HB.blockHashToHex blockHash)
        Just block -> do
          let mSpentTx = find (\tx -> HT.txHash tx == spentTxHash) (HB.blockTxns block)
          case mSpentTx of
            Nothing -> pure False
            Just spentTx -> do
              checkResult <- checkTxOut addr $ (HT.txOut spentTx) !! (fromIntegral spentOutputIndex)
              pure checkResult

-- | Checks given TxOut wheather it contains given address.
checkTxOut :: (MonadIO m, PlatformNatives) => EgvAddress -> HT.TxOut -> m Bool
checkTxOut (BtcAddress (HA.WitnessPubKeyAddress pkh)) txO = case HS.decodeOutputBS $ HT.scriptOutput txO of
  Left e -> do
    logWrite $ "Could not decode transaction output " <> (showt e)
    pure False
  Right output -> case output of
    HS.PayWitnessPKHash h -> if h == pkh then pure True else pure False
    _ -> pure False
checkTxOut (BtcAddress (HA.WitnessScriptAddress sh)) txO = case HS.decodeOutputBS $ HT.scriptOutput txO of
  Left e -> do
    logWrite $ "Could not decode transaction output " <> (showt e)
    pure False
  Right output -> case output of
    HS.PayWitnessScriptHash h -> if h == sh then pure True else pure False
    _ -> pure False
