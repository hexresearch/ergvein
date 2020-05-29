module Ergvein.Wallet.Scan (
    accountDiscovery
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Dependent.Map (DMap, DSum((:=>)))
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
import Ergvein.Wallet.Util
import Ergvein.Wallet.Scan.BTC
import Ergvein.Wallet.Scan.ERGO

import qualified Data.Dependent.Map                 as DM
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
  activeCurrencies <- _pubStorage'activeCurrencies <$> getPubStorage
  updatedPubStoragesE <- scan pubStorages activeCurrencies
  authD <- getAuthInfo
  let updatedAuthE = traceEventWith (const "Account discovery finished") <$>
        flip pushAlways updatedPubStoragesE $ \updatedPubStorages -> do
          auth <- sampleDyn authD
          pure $ Just $ auth
            & authInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages .~ updatedPubStorages
            & authInfo'isUpdate .~ True
  setAuthInfoE <- setAuthInfo updatedAuthE
  storeWallet setAuthInfoE

-- | Performs BIP44 account discovery algorithm for active currencies.
-- Returns event with updated PubStorage as a result.
scan :: MonadFront t m => CurrencyPubStorages -> [Currency] -> m (Event t CurrencyPubStorages)
scan currencyPubStorages activeCurrencies = do
  scanEvents <- traverse (applyScan currencyPubStorages) activeCurrencies -- [Event t (DSum CurrencyTag CurrencyPubStorage)]
  let scanEvents' = map (fmap $ DM.fromList . (:[])) scanEvents -- [Event t (DMap CurrencyTag CurrencyPubStorage)]
      scanEvents'' = mergeWith DM.union scanEvents' -- Event t (DMap CurrencyTag CurrencyPubStorage)
  scannedCurrencyPubStoragesD <- foldDyn DM.union DM.empty scanEvents''
  let finishedE = flip push (updated scannedCurrencyPubStoragesD) $ \updatedCurrencyPubStorage -> do
        pure $ if DM.size updatedCurrencyPubStorage == length activeCurrencies then Just $ updatedCurrencyPubStorage else Nothing
  pure finishedE

-- | Applies scan for a certain currency then returns event with result.
applyScan :: MonadFront t m => CurrencyPubStorages -> Currency -> m (Event t (DSum CurrencyTag CurrencyPubStorage))
applyScan currencyPubStorages currency = case currency of
  BTC -> case DM.lookup BTCTag currencyPubStorages of
    Nothing -> err
    Just currencyPubStorage -> do
      scanE <- scanBTC currencyPubStorage
      pure $ (BTCTag :=>) <$> scanE
  ERGO -> case DM.lookup ERGTag currencyPubStorages of
    Nothing -> err
    Just currencyPubStorage -> do
      scanE <- scanERG currencyPubStorage
      pure $ (ERGTag :=>) <$> scanE
  where err = fail $ "Could not find " ++ (T.unpack $ currencyName currency) ++ " in public storage"
