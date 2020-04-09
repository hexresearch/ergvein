module Ergvein.Wallet.Page.Initial(
    initialPage
  , initialAuthedPage
  ) where

import Data.Text (unpack)

import Control.Lens
import Control.Monad.IO.Class
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Camera
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Initial
import Ergvein.Wallet.Localization.Storage
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Password
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Widget.GraphPinCode
import Ergvein.Wallet.Wrapper

import qualified Data.Map.Strict       as M
import qualified Data.Map.Merge.Strict as MM
import qualified Data.IntMap.Strict    as MI

data GoPage = GoSeed | GoRestore

initialPage :: MonadFrontBase t m => m ()
initialPage = do
  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage ss
  where
    noWalletsPage = wrapperSimple True $ divClass "initial-options grid1" $ noWallets
    noWallets = do
      newE <- fmap (GoSeed <$) $ outlineButton IPSCreate
      restoreE <- fmap (GoRestore <$) $ outlineButton IPSRestore
      let goE = leftmost [newE, restoreE]
      void $ nextWidget $ ffor goE $ \go -> Retractable {
          retractableNext = case go of
            GoSeed -> mnemonicPage
            GoRestore -> seedRestorePage
        , retractablePrev = Just $ pure initialPage
        }
    hasWalletsPage ss = do
      mname <- getLastStorage
      maybe (selectWalletsPage ss) loadWalletPage mname
    selectWalletsPage ss = wrapperSimple True $ divClass "initial-options grid1" $ do
      h4 $ localizedText IPSSelectWallet
      flip traverse_ ss $ \name -> do
        btnE <- outlineButton name
        void $ nextWidget $ ffor btnE $ const $ Retractable {
            retractableNext = loadWalletPage name
          , retractablePrev = Just $ pure $ selectWalletsPage ss
          }
      h4 $ localizedText IPSOtherOptions
      noWallets
    loadWalletPage name = do
      passE <- askPasswordPage name
      mOldAuthE <- performEvent $ loadAuthInfo name <$> passE
      oldAuthE <- handleDangerMsg mOldAuthE
      mAuthE <- performEvent $ generateMissingPrvKeys <$> oldAuthE
      authE <- handleDangerMsg mAuthE
      void $ setAuthInfo $ Just <$> authE

-- | Generates new private keys until their number is equal to the number of public keys.
generateMissingPrvKeys :: MonadIO m => (AuthInfo, Password) -> m (Either StorageAlert AuthInfo)
generateMissingPrvKeys (authInfo, pass) = do
  let encryptedPrvStorage = view (authInfo'storage . storage'encryptedPrivateStorage) authInfo
  case decryptPrivateStorage encryptedPrvStorage pass of
    Left err -> pure $ Left err
    Right decryptedPrivateStorage -> do
      let updatedPrivateStorage = set privateStorage'privateKeys updatedPrvKeystore decryptedPrivateStorage
      encryptPrivateStorageResult <- encryptPrivateStorage updatedPrivateStorage pass
      case encryptPrivateStorageResult of
        Left err -> pure $ Left err
        Right encryptedUpdatedPrivateStorage -> pure $ Right $ set (authInfo'storage . storage'encryptedPrivateStorage) encryptedUpdatedPrivateStorage authInfo
      where
        prvKeystore = view privateStorage'privateKeys decryptedPrivateStorage
        pubKeystore = view (authInfo'storage . storage'publicKeys) authInfo
        pubKeysNumber = M.map (\keychain -> (
            MI.size $ egvPubKeyсhain'external keychain,
            MI.size $ egvPubKeyсhain'internal keychain
          )) pubKeystore
        updatedPrvKeystore =
          MM.merge
          MM.dropMissing
          MM.dropMissing
          (MM.zipWithMatched generateMissingPrvKeysHelper)
          prvKeystore
          pubKeysNumber

generateMissingPrvKeysHelper ::
  Currency
  -> EgvPrvKeyсhain -- ^ Private keychain
  -> (Int, Int)     -- ^ Total number of external and internal private keys respectively that should be stored in keychain
  -> EgvPrvKeyсhain -- ^ Updated private keychain
generateMissingPrvKeysHelper currency prvKeychain (goalExternalKeysNum, goalInternalKeysNum) =
  EgvPrvKeyсhain masterPrvKey updatedExternalPrvKeys updatedInternalPrvKeys
  where
    currentExternalKeys = egvPrvKeyсhain'external prvKeychain
    currentInternalKeys = egvPrvKeyсhain'internal prvKeychain
    masterPrvKey = egvPrvKeyсhain'master prvKeychain
    updatedExternalPrvKeys = if MI.size currentExternalKeys >= goalExternalKeysNum
                             then currentExternalKeys
                             else MI.union currentExternalKeys
                                    (MI.fromList [(keyIndex, derivePrvKey masterPrvKey External (fromIntegral keyIndex)) |
                                    keyIndex <- [(MI.size currentExternalKeys)..(goalExternalKeysNum - 1)]])
    updatedInternalPrvKeys = if MI.size currentInternalKeys >= goalInternalKeysNum
                             then currentInternalKeys
                             else MI.union currentInternalKeys
                                    (MI.fromList [(keyIndex, derivePrvKey masterPrvKey Internal (fromIntegral keyIndex)) |
                                    keyIndex <- [(MI.size currentInternalKeys)..(goalInternalKeysNum - 1)]])

initialAuthedPage :: MonadFront t m => m ()
initialAuthedPage = wrapperSimple True $ divClass "main-page" $ do
  anon_name <- getWalletName
  h4 $ text $ "Congrats " <> anon_name <> "! You've made it!"
  logoutE <- row . outlineButton $ ("Logout" :: Text)
  void $ setAuthInfo $ Nothing <$ logoutE
