module Ergvein.Wallet.Scan(
    accountDiscovery
  ) where

import Ergvein.Wallet.Storage.Keys (derivePubKey)
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.AuthInfo
import Ergvein.Types.Transaction (BlockHeight)
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Network.Haskoin.Block (Block, genesisBlock)

import qualified Data.IntMap.Strict as MI
import qualified Data.Map.Strict    as M

accountDiscovery :: MonadFront t m => m ()
accountDiscovery = do
  logWrite "Key scanning started"
  pubKeystore <- getPublicKeystore
  updatedPubKeystoreE <- scanKeys pubKeystore
  mauthD <- getAuthInfoMaybe
  let updatedAuthE = traceEventWith (const "Key scanning finished") <$> flip pushAlways updatedPubKeystoreE $ \store -> do
        mauth <- sample . current $ mauthD
        case mauth of
          Nothing -> fail "accountDiscovery: not authorized"
          Just auth -> pure $ Just $ authInfo'storage . storage'publicKeys .~ store $ authInfo'isUpdate .~ True $ auth
  setAuthInfoE <- setAuthInfo updatedAuthE
  storeWallet $ () <$ setAuthInfoE

scanKeys :: MonadFront t m => PublicKeystore -> m (Event t PublicKeystore)
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
applyScan :: MonadFront t m => PublicKeystore -> Currency -> m (Event t (Currency, EgvPubKeyсhain))
applyScan pubKeystore currency = scanCurrencyKeys currency (getKeychain currency pubKeystore )
  where getKeychain currency pubKeystore = pubKeystore M.! currency

scanCurrencyKeys :: MonadFront t m => Currency -> EgvPubKeyсhain -> m (Event t (Currency, EgvPubKeyсhain))
scanCurrencyKeys currency keyChain = mdo
  buildE <- getPostBuild
  nextE <- delay 0 $ leftmost [newE, buildE]
  gapD <- holdDyn 0 gapE
  nextKeyIndexD <- holdDyn initKeyChainSize nextKeyIndexE
  newKeyChainD <- foldDyn addKey keyChain nextKeyE
  filterAddressE <- filterAddress nextKeyE
  getBlockE <- getBlocks filterAddressE
  storedE <- storeNewTransactions getBlockE
  let masterPubKey = egvPubKeyсhain'master keyChain
      initKeyChainSize = MI.size $ egvPubKeyсhain'external keyChain
      newE = flip push storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap > gapLimit then Nothing else Just ()
      gapE = flip pushAlways storedE $ \i -> do
        gap <- sample . current $ gapD
        pure $ if i == 0 && gap <= gapLimit then gap + 1 else 0
      nextKeyE = flip push nextE $ \_ -> do
        gap <- sample . current $ gapD
        nextKeyIndex <- sample . current $ nextKeyIndexD
        pure $ if gap > gapLimit then Nothing else Just $ generateNextKey masterPubKey External nextKeyIndex
      nextKeyIndexE = flip pushAlways nextKeyE $ \_ -> do
        nextKeyIndex <- sample . current $ nextKeyIndexD
        pure $ nextKeyIndex + 1
      finishedE = flip push gapE $ \g -> do
        newKeyChain <- sample . current $ newKeyChainD
        pure $ if g > gapLimit then Just $ (currency, newKeyChain) else Nothing
  pure finishedE

generateNextKey :: EgvXPubKey -> KeyPurpose -> Int -> (Int, EgvXPubKey)
generateNextKey master purpose index = (index, derivedXPubKey)
  where currency = egvXPubCurrency master
        derivedXPubKey = derivePubKey master purpose (fromIntegral index)

addKey :: (Int, EgvXPubKey) -> EgvPubKeyсhain -> EgvPubKeyсhain
addKey (index, key) (EgvPubKeyсhain master external internal) = EgvPubKeyсhain master (MI.insert index key external) internal

-- FIXME
filterAddress :: MonadFront t m => Event t (Int, EgvXPubKey) -> m (Event t [BlockHeight])
filterAddress addrE = pure $ filterAddressMock <$> addrE
  where filterAddressMock (idx, addr) = if idx < 5 then [1] else []

-- FIXME
getBlocks :: MonadFront t m => Event t [BlockHeight] -> m (Event t [Block])
getBlocks blockHeightE = pure $ getBlocksMock <$> blockHeightE
  where getBlocksMock bhs = if null bhs then [] else [genesisBlock $ getCurrencyNetwork BTC]

-- FIXME
storeNewTransactions :: MonadFront t m => Event t [Block] -> m (Event t Int)
storeNewTransactions valE = pure $ storeNewTransactionsMock <$> valE
  where storeNewTransactionsMock blocks = if null blocks then 0 else 1
