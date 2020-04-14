{-# LANGUAGE NumericUnderscores #-}
module Ergvein.Wallet.Scan (
    accountDiscovery
  ) where

import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeyсhain)
import Network.Haskoin.Block (Block, BlockHash, genesisBlock)

import qualified Data.IntMap.Strict          as MI
import qualified Data.Map.Strict             as M
import qualified Ergvein.Wallet.Filters.Scan as Filters

-- | Loads current PubStorage, performs BIP44 account discovery algorithm and
-- stores updated PubStorage to the wallet file.
accountDiscovery :: MonadFront t m => m ()
accountDiscovery = do
  logWrite "Key scanning started"
  pubKeystore <- getPubStorage
  let showPubKeystoreDiff updatedPubKeystore =
        "Discovered new BTC keys: " ++ show (btcAddressesCount updatedPubKeystore - btcAddressesCount pubKeystore) ++ "\n" ++
        "Discovered new ERGO keys: " ++ show (ergAddressesCount updatedPubKeystore - ergAddressesCount pubKeystore)
      btcAddressesCount s = getExternalPubkeysCount BTC s
      ergAddressesCount s = getExternalPubkeysCount ERGO s
      getExternalPubkeysCount currency keystore = MI.size $ pubKeystore'external (keystore M.! currency)
  updatedPubKeystoreE <- traceEventWith showPubKeystoreDiff <$> scanKeys pubKeystore
  authD <- getAuthInfo
  let updatedAuthE = traceEventWith (const "Key scanning finished") <$> flip pushAlways updatedPubKeystoreE $ \store -> do
        auth <- sample . current $ authD
        pure $ Just $ auth
          & authInfo'storage . storage'pubKeys .~ store
          & authInfo'isUpdate .~ True
  setAuthInfoE <- setAuthInfo updatedAuthE
  storeWallet setAuthInfoE

-- Gets old PubStorage, performs BIP44 account discovery algorithm for all currencies
-- then returns Event with updated PubStorage.
scanKeys :: MonadFront t m => PubStorage -> m (Event t PubStorage)
scanKeys pubKeystore = do
  scanEvents <- traverse (applyScan pubKeystore) allCurrencies
  let scanEvents' = [(M.fromList . (: []) <$> e) | e <- scanEvents]
      scanEvents'' = mergeWith M.union scanEvents'
  newPubKeystoreD <- foldDyn M.union M.empty scanEvents''
  let newPubKeystoreDUpdatedE = updated newPubKeystoreD
      allFinishedE = flip push newPubKeystoreDUpdatedE $ \updatedKeystore -> do
        pure $ if M.size updatedKeystore == length allCurrencies then Just $ updatedKeystore else Nothing
  pure allFinishedE

-- TODO: use M.lookup instead of M.! and show error msg if currency not found
applyScan :: MonadFront t m => PubStorage -> Currency -> m (Event t (Currency, PubKeystore))
applyScan pubKeystore currency = scanCurrencyKeys currency (getKeystore currency pubKeystore)
  where getKeystore cur pubKeystore' = pubKeystore' M.! cur

scanCurrencyKeys :: MonadFront t m => Currency -> PubKeystore -> m (Event t (Currency, PubKeystore))
scanCurrencyKeys currency keystore = mdo
  buildE <- getPostBuild
  nextE <- waitFilters currency =<< delay 0 (leftmost [newE, buildE])
  gapD <- holdDyn 0 gapE
  nextKeyIndexD <- holdDyn (if initKeystoreSize > gapLimit then initKeystoreSize - gapLimit else 0) nextKeyIndexE
  newKeystoreD <- foldDyn (addXPubKeyToKeyсhain External) keystore nextKeyE
  filterAddressE <- filterAddress nextKeyE
  getBlockE <- getBlocks filterAddressE
  storedE <- storeNewTransactions getBlockE
  let masterPubKey = pubKeystore'master keystore
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
        pure $ if g >= gapLimit then Just $ (currency, newKeystore) else Nothing
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
filterAddress e = performFilters $ ffor e $ \(_, pk) -> Filters.filterAddress $ egvXPubKeyToEgvAddress pk

-- FIXME
getBlocks :: MonadFront t m => Event t [BlockHash] -> m (Event t [Block])
getBlocks blockHeightE = pure $ getBlocksMock <$> blockHeightE
  where getBlocksMock bhs = if null bhs then [] else [genesisBlock $ getBtcNetwork $ getCurrencyNetwork BTC]

-- FIXME
storeNewTransactions :: MonadFront t m => Event t [Block] -> m (Event t Int)
storeNewTransactions valE = pure $ storeNewTransactionsMock <$> valE
  where storeNewTransactionsMock blocks = if null blocks then 0 else 1
