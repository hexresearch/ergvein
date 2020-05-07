{-# LANGUAGE NumericUnderscores #-}
module Ergvein.Wallet.Scan (
    accountDiscovery
  ) where

import Network.Haskoin.Block (Block, BlockHash, genesisBlock)

import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Wallet.Blocks.BTC
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)

import qualified Data.IntMap.Strict          as MI
import qualified Data.Map.Strict             as M
import qualified Ergvein.Wallet.Filters.Scan as Filters
import qualified Data.Text                   as T


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
          auth <- sample . current $ authD
          pure $ Just $ auth
            & authInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages .~ updatedPubStorages
            & authInfo'isUpdate .~ True
  setAuthInfoE <- setAuthInfo updatedAuthE
  storeWallet setAuthInfoE

-- Gets old CurrencyPubStorages, performs BIP44 account discovery algorithm for all currencies
-- then returns event with updated PubStorage.
scan :: MonadFront t m => CurrencyPubStorages -> [Currency] -> m (Event t CurrencyPubStorages)
scan currencyPubStorages curs = do
  scanEvents <- traverse (applyScan currencyPubStorages) curs
  let scanEvents' = [(M.fromList . (: []) <$> e) | e <- scanEvents]
      scanEvents'' = mergeWith M.union scanEvents'
  scannedCurrencyPubStoragesD <- foldDyn M.union M.empty scanEvents''
  let allFinishedE = flip push (updated scannedCurrencyPubStoragesD) $ \updatedCurrencyPubStorage -> do
        pure $ if M.size updatedCurrencyPubStorage == length allCurrencies then Just $ updatedCurrencyPubStorage else Nothing
  pure allFinishedE

-- Applies scan for a certain currency then returns event with result.
applyScan :: MonadFront t m => CurrencyPubStorages -> Currency -> m (Event t (Currency, CurrencyPubStorage))
applyScan currencyPubStorages currency =
  case M.lookup currency currencyPubStorages of
    Nothing -> fail $ "Could not find currency: " ++ (T.unpack $ currencyName currency)
    Just currencyPubStorage -> scanCurrency currency currencyPubStorage

scanCurrency :: MonadFront t m => Currency -> CurrencyPubStorage -> m (Event t (Currency, CurrencyPubStorage))
scanCurrency currency currencyPubStorage = mdo
  buildE <- getPostBuild
  nextE <- waitFilters currency =<< delay 0 (leftmost [newE, buildE])
  gapD <- holdDyn 0 gapE
  nextKeyIndexD <- holdDyn (if initKeystoreSize > gapLimit then initKeystoreSize - gapLimit else 0) nextKeyIndexE
  -- добавить сюда добавление транзакций в CurrencyPubStorage
  -- newKeystoreD переименовать в newCurrencyPubStorageD
  newKeystoreD <- foldDyn (addXPubKeyToKeystore External) keystore nextKeyE
  filterAddressE <- filterAddress nextKeyE
  getBlockE <- requestBTCBlocksWaitRN filterAddressE
  storedE <- storeNewTransactions getBlockE
  let keystore = _currencyPubStorage'pubKeystore currencyPubStorage
      masterPubKey = pubKeystore'master keystore
      initKeystoreSize = MI.size $ pubKeystore'external keystore
      newE = flip push storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap >= gapLimit then Nothing else Just ()
      gapE = flip pushAlways storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap < gapLimit then gap + 1 else 0
      nextKeyE = flip push nextE $ \_ -> do
        gap <- sample . current $ gapD
        nextKeyIndex <- sample . current $ nextKeyIndexD
        pure $ if gap >= gapLimit then Nothing else Just $ (nextKeyIndex, derivePubKey masterPubKey External (fromIntegral nextKeyIndex))
      nextKeyIndexE = flip pushAlways nextKeyE $ \_ -> do
        nextKeyIndex <- sample . current $ nextKeyIndexD
        pure $ nextKeyIndex + 1
      finishedE = flip push gapE $ \g -> do
        newKeystore <- sample . current $ newKeystoreD
        pure $ if g >= gapLimit then Just $ (currency, CurrencyPubStorage newKeystore M.empty) else Nothing
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

filterAddress :: MonadFront t m => Event t (Int, EgvXPubKey) -> m (Event t [BlockHash])
filterAddress e = performFork $ ffor e $ \(_, pk) -> Filters.filterAddress $ egvXPubKeyToEgvAddress pk

-- FIXME
storeNewTransactions :: MonadFront t m => Event t [Block] -> m (Event t Int)
storeNewTransactions valE = pure $ storeNewTransactionsMock <$> valE
  where storeNewTransactionsMock blocks = if null blocks then 0 else 1
