module Ergvein.Core.Store.Env(
    StoreEnv(..)
  , StoreM
  , HasStoreEnv(..)
  , newStoreEnv
  , runStore
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Reader
import Crypto.Random.Types
import Data.Foldable (traverse_)
import Data.Text (Text)
import Ergvein.Core.Store.Monad
import Ergvein.Core.Store.Util
import Ergvein.Types
import Reflex
import Reflex.ExternalRef
import Sepulcas.Native

import qualified Data.Vector as V
import qualified Data.Map.Strict as M

data StoreEnv t = StoreEnv {
  senv'authRef         :: !(ExternalRef t WalletInfo)
, senv'storeMutex      :: !(MVar ())
, senv'storeChan       :: !(TChan (Text, WalletInfo))
}

type StoreM t m = ReaderT (StoreEnv t) m

class Monad m => HasStoreEnv t m | m -> t where
  getStoreEnv :: m (StoreEnv t)

instance Monad m => HasStoreEnv t (StoreM t m) where
  getStoreEnv = ask
  {-# INLINE getStoreEnv #-}

instance {-# OVERLAPPABLE #-} (HasStoreEnv t m, MonadStorageConstr t m, HasStoreDir m, MonadRandom (Performable m)) => MonadStorage t m where
  getEncryptedPrvStorage = fmap (_storage'encryptedPrvStorage . _walletInfo'storage) $ readExternalRef =<< fmap senv'authRef getStoreEnv
  {-# INLINE getEncryptedPrvStorage #-}
  getAddressByCurIx cur i = do
    currMap <- fmap (_pubStorage'currencyPubStorages . _storage'pubStorage . _walletInfo'storage) $ readExternalRef =<< fmap senv'authRef getStoreEnv
    let mXPubKey = (flip (V.!?) i) . pubKeystore'external . _currencyPubStorage'pubKeystore =<< M.lookup cur currMap
    case mXPubKey of
      Nothing -> fail "NOT IMPLEMENTED" -- TODO: generate new address here
      Just (EgvPubKeyBox key _ _) ->
        let k = case key of
              ErgXPubKey k' _ -> k'
              BtcXPubKey k' _ -> k'
        in pure $ xPubExport (getCurrencyNetwork cur) k
  {-# INLINE getAddressByCurIx #-}
  getWalletName = fmap (_storage'walletName . _walletInfo'storage) $ readExternalRef =<< fmap senv'authRef getStoreEnv
  {-# INLINE getWalletName #-}
  getPubStorage = fmap (_storage'pubStorage . _walletInfo'storage) $ readExternalRef =<< fmap senv'authRef getStoreEnv
  {-# INLINE getPubStorage #-}
  getPubStorageD = do
    walletInfoD <- externalRefDynamic =<< fmap senv'authRef getStoreEnv
    pure $ ffor walletInfoD $ \ai -> ai ^. walletInfo'storage. storage'pubStorage
  {-# INLINE getPubStorageD #-}
  storeWallet caller e = do
    ref <-  fmap senv'authRef getStoreEnv
    performEvent $ ffor e $ \_ -> do
        walletInfo <- readExternalRef ref
        let storage = _walletInfo'storage walletInfo
        let eciesPubKey = _walletInfo'eciesPubKey walletInfo
        saveStorageToFile caller eciesPubKey storage
  {-# INLINE storeWallet #-}

  modifyPubStorage caller fe = do
    authRef   <- fmap senv'authRef getStoreEnv
    chan      <- fmap senv'storeChan getStoreEnv
    performEvent $ ffor fe $ \f -> do
      mai <- modifyExternalRefMaybe authRef $ \ai ->
        let mps' = f (ai ^. walletInfo'storage . storage'pubStorage)
        in (\a -> (a, a)) . (\ps' -> ai & walletInfo'storage . storage'pubStorage .~ ps') <$> mps'
      liftIO $ atomically $ traverse_ (writeTChan chan . (caller, )) mai
  {-# INLINE modifyPubStorage #-}
  getStoreMutex = fmap senv'storeMutex getStoreEnv
  {-# INLINE getStoreMutex #-}
  getStoreChan = fmap senv'storeChan getStoreEnv
  {-# INLINE getStoreChan #-}

newStoreEnv :: MonadIO m => ExternalRef t WalletInfo -> m (StoreEnv t)
newStoreEnv authRef = StoreEnv
  <$> pure authRef
  <*> liftIO (newMVar ())
  <*> liftIO newTChanIO

runStore :: StoreEnv t -> StoreM t m a -> m a
runStore = flip runReaderT
