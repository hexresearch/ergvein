{-# LANGUAGE NumericUnderscores #-}
module Ergvein.Wallet.Scan (
    accountDiscovery
  ) where

import Data.ByteString (ByteString)
import Data.List
import Ergvein.Aeson
import Ergvein.Crypto.Keys
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
  -- processKeyE <- waitFilters currency =<< delay 0 (leftmost [nextKeyE, buildE])
  processKeyE <- delay 0 (leftmost [nextKeyE, (0, fstKey) <$ buildE])
  gapD <- holdDyn 0 gapE
  currentKeyD <- holdDyn (0, fstKey) nextKeyE
  newKeystoreD <- foldDyn (addXPubKeyToKeystore External) emptyPubKeystore processKeyE
  newTxsD <- foldDyn M.union emptyTxs getTxsE
  filterAddressE <- filterAddress $ (egvXPubKeyToEgvAddress . snd) <$> nextKeyE
  getBlocksE <- requestBTCBlocksWaitRN filterAddressE
  getTxsE <- getTxs $ attachPromptlyDynWith (\(_, key) blocks -> (egvXPubKeyToEgvAddress key, blocks)) currentKeyD getBlocksE
  let fstKey = derivePubKey masterPubKey External (fromIntegral 0)
      pubKeystore = _currencyPubStorage'pubKeystore currencyPubStorage
      masterPubKey = pubKeystore'master pubKeystore
      emptyPubKeystore = PubKeystore masterPubKey MI.empty MI.empty
      emptyTxs = M.empty
      -- TODO: seems like nextKeyE and gapE fires simultaneously, this is confusing.
      nextKeyE = flip push getTxsE $ \txs -> do
        gap <- sampleDyn gapD
        currentKeyIndex <- fmap fst (sampleDyn currentKeyD)
        let nextKeyIndex = currentKeyIndex + 1
        pure $ if null txs && gap >= gapLimit
          then Nothing
          else Just $ (nextKeyIndex, derivePubKey masterPubKey External (fromIntegral nextKeyIndex))
      gapE = flip pushAlways getTxsE $ \txs -> do
        gap <- sampleDyn gapD
        pure $ if null txs && gap < gapLimit then gap + 1 else 0
      finishedE = traceEventWith (T.unpack . encodeJson) $ flip push gapE $ \gap -> do
      -- finishedE = flip push gapE $ \g -> do
        newKeystore <- sampleDyn newKeystoreD
        newTxs <- sampleDyn newTxsD
        pure $ if gap >= gapLimit then Just $ (currency, CurrencyPubStorage newKeystore newTxs) else Nothing
  pure finishedE

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
getAddrTxsFromBlocks :: HasBlocksStorage m => EgvAddress -> [HB.Block] -> m (M.Map TxId EgvTx)
getAddrTxsFromBlocks addr blocks = do
  txMaps <- traverse (getAddrTxsFromBlock addr) blocks
  pure $ M.unions txMaps

getAddrTxsFromBlock :: HasBlocksStorage m => EgvAddress -> HB.Block -> m (M.Map TxId EgvTx)
getAddrTxsFromBlock addr block = do
  checkResults <- traverse (checkAddrTx addr) txs
  let filteredTxs = fst $ unzip $ filter (not . snd) (zip txs checkResults)
  pure $ M.fromList [(HT.txHashToHex $ HT.txHash tx, BtcTx tx) | tx <- filteredTxs]
  where txs = HB.blockTxns block

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: HasBlocksStorage m => EgvAddress -> HT.Tx -> m Bool
checkAddrTx addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HT.txIn tx)
  let checkTxOutputsResults = map (checkTxOut addr) (HT.txOut tx)
      concatResults = foldr (||) False
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults

-- | Checks given TxIn wheather it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxIn :: HasBlocksStorage m => EgvAddress -> HT.TxIn -> m Bool
checkTxIn addr txIn = do
  let prevOutput = HT.prevOutput txIn
      txHash = HT.outPointHash prevOutput
      outputIndex = HT.outPointIndex prevOutput
  -- TODO: get tx by id and analyze corresponding output.
  -- prevTx <- getTxById txHash
  pure False

-- | Checks given TxOut wheather it contains given address.
checkTxOut :: EgvAddress -> HT.TxOut -> Bool
checkTxOut (BtcAddress (HA.WitnessPubKeyAddress pkh)) txO = case HS.decodeOutputBS $ HT.scriptOutput txO of
  Left _ -> False -- TODO: show error here somehow?
  Right output -> case output of
    HS.PayWitnessPKHash h -> if h == pkh then True else False
    _ -> False
checkTxOut (BtcAddress (HA.WitnessScriptAddress pkh)) txO = case HS.decodeOutputBS $ HT.scriptOutput txO of
  Left _ -> False -- TODO: show error here somehow?
  Right output -> case output of
    HS.PayWitnessScriptHash h -> if h == pkh then True else False
    _ -> False
