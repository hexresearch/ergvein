module Ergvein.Core.Store.Crypto(
    withWallet
  , modifyPrvStorage
  ) where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Data.Proxy

import Ergvein.Core.Password
import Ergvein.Core.Store.Constants
import Ergvein.Core.Store.Util
import Ergvein.Core.Wallet.Monad
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.WalletInfo
import Reflex.Flunky
import Reflex.Localize.Class
import Sepulcas.Alert

import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- | Requests password, runs a callback against decoded wallet and returns the result
-- The callback is in 'Performable m'
-- The resulting event does not fire if there is an error with decoding or validating
withWallet :: forall t m a . (MonadWallet t m, PasswordAsk t m, LocalizedPrint StorageAlert)
  => Event t (PrvStorage -> Performable m a)    -- ^ Event with a callback
  -> m (Event t a)                              -- ^ results of applying the callback to the wallet
withWallet reqE = do
  walletName  <- getWalletName
  walletInfoD <- getWalletInfo
  widgD <- holdDyn Nothing $ Just <$> reqE
  isPlainE <- performEvent $ ffor reqE $ const $ _walletInfo'isPlain <$> sampleDyn walletInfoD
  let passE' = ("" <$) $ ffilter id isPlainE
  passE'' <- requestPasssword $ walletName <$ (ffilter not isPlainE)
  let passE = leftmost [passE', passE'']
  mutex <- getStoreMutex
  let goE = attachWithMaybe (\mwid pass -> (pass, ) <$> mwid) (current widgD) passE
  eresE <- performEvent $ ffor goE $ \(pass, f) -> do
    eprv <- decryptAndValidatePrvStorage (Proxy :: Proxy m) mutex pass walletInfoD
    either (pure . Left) (fmap Right . f) eprv
  handleDangerMsg eresE

-- | More specialized version. Provide an update function to manually update the private storage
modifyPrvStorage :: forall t m . (MonadWallet t m, PasswordAsk t m, LocalizedPrint StorageAlert)
  => Event t (PrvStorage -> Performable m (Maybe PrvStorage)) -- ^ updater. Nothing == no update
  -> m (Event t ())
modifyPrvStorage updE = do
  walletName <- getWalletName
  walletInfoD <- getWalletInfo
  updD <- holdDyn Nothing $ Just <$> updE
  isPlainE <- performEvent $ ffor updE $ const $ fmap _walletInfo'isPlain $ sampleDyn walletInfoD
  let passE' = ("" <$) $ ffilter id isPlainE
  passE'' <- requestPasssword $ walletName <$ (ffilter not isPlainE)
  let passE = leftmost [passE', passE'']
  mutex <- getStoreMutex
  let goE = attachWithMaybe (\mupd pass -> (pass, ) <$> mupd) (current updD) passE
  errE <- performEvent $ ffor goE $ \(pass, upd) -> do
    liftIO $ takeMVar mutex                                       -- We want the next part to not interfere with any store changes
    let withMutexRelease a = liftIO $ putMVar mutex () >> pure a  -- release the mutex and return the value
    ai <- sampleDyn walletInfoD
    let WalletStorage eps pub _ = _walletInfo'storage ai
    let eciesPubKey = _walletInfo'eciesPubKey ai
    either' (decryptPrvStorage eps pass) (withMutexRelease . Left) $ \prv -> do
      mprv <- upd prv
      maybe' mprv (withMutexRelease $ Right ()) $ \prv' -> do
        eeps <- encryptPrvStorage prv' pass
        either' eeps (withMutexRelease . Left) $ \eps' -> do
          let wallet = WalletStorage eps' pub walletName
          err <- saveStorageSafelyToFile "modifyPrvStorage" eciesPubKey wallet
          either' err (withMutexRelease . Left) $ const $ do
            setWalletInfoNow (Proxy :: Proxy m) $ Just $ ai {_walletInfo'storage = wallet}
            withMutexRelease $ Right ()
  handleDangerMsg errE
  where
    maybe' m n j = maybe n j m
    either' e l r = either l r e

-- | Decrypt the private storage and run validation routines before returning it
-- This is important because this is the only point the app has access to the private storage
-- and thus this is the only place we can update seamlessly. E.g. generate missing keys
decryptAndValidatePrvStorage :: forall t m . MonadWallet t m
  => Proxy m                  -- ^ A proxy, to bind the m
  -> MVar ()                  -- ^ Storage writing mutex
  -> Password                 -- ^ User's password
  -> Dynamic t WalletInfo   -- ^ A ref to WalletInfo. We update WalletInfo if the validation stage changed the PrvStorage
  -> Performable m (Either StorageAlert PrvStorage) -- ^ Decoded PrvStorage. Use in withWallet only
decryptAndValidatePrvStorage proxy mutex pass walletInfoD = do
  ai <- sampleDyn walletInfoD
  let WalletStorage eps pub walletName = _walletInfo'storage ai
  let eciesPubKey = _walletInfo'eciesPubKey ai
  let pubKeysNumber = M.map (\currencyPubStorage -> (
          V.length $ pubKeystore'external (view currencyPubStorage'pubKeystore currencyPubStorage),
          V.length $ pubKeystore'internal (view currencyPubStorage'pubKeystore currencyPubStorage)
        )) $ _pubStorage'currencyPubStorages pub
  let updatedPrvKeystore currencyPrvStorages =
        MM.merge
        MM.dropMissing
        MM.dropMissing
        (MM.zipWithMatched generateMissingPrvKeysHelper)
        currencyPrvStorages
        pubKeysNumber

  liftIO $ takeMVar mutex
  either' (decryptPrvStorage eps pass) (withMutexRelease . Left) $ \prv -> do
    let prvKeysNumber = M.map (\(CurrencyPrvStorage keystore _) -> (
            V.length $ prvKeystore'external keystore
          , V.length $ prvKeystore'internal keystore
          )) $ _prvStorage'currencyPrvStorages prv
    let diff = M.differenceWith (\a b -> if a == b then Nothing else Just a) pubKeysNumber prvKeysNumber
    if M.null diff then withMutexRelease $ Right prv else do
      let updatedPrvStorage = set prvStorage'currencyPrvStorages (updatedPrvKeystore $ _prvStorage'currencyPrvStorages prv) prv
      eeps <- encryptPrvStorage updatedPrvStorage pass
      either' eeps (withMutexRelease . Left) $ \eps' -> do
        let wallet = WalletStorage eps' pub walletName
        err <- saveStorageSafelyToFile "decryptAndValidatePrvStorage" eciesPubKey wallet
        either' err (withMutexRelease . Left) $ const $ do
          setWalletInfoNow proxy $ Just ai {_walletInfo'storage = wallet}
          withMutexRelease $ Right updatedPrvStorage
  where
    withMutexRelease a = liftIO $ putMVar mutex () >> pure a  -- release the mutex and return the value
    either' e l r = either l r e
