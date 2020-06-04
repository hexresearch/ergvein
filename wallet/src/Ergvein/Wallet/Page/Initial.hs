{-# OPTIONS_GHC -Wall #-}

module Ergvein.Wallet.Page.Initial(
    initialPage
  , initialAuthedPage
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Initial
import Ergvein.Wallet.Localization.Storage
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Password
import Ergvein.Wallet.Page.Seed
import Ergvein.Wallet.Storage.AuthInfo
import Ergvein.Wallet.Storage.Keys
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Wrapper

import qualified Data.Dependent.Map    as DM
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Map.Strict       as M
import qualified Data.Vector           as V

data GoPage = GoSeed | GoRestore

initialPage :: MonadFrontBase t m => m ()
initialPage = do
  logWrite "Initial page rendering"
  ss <- listStorages
  if null ss then noWalletsPage else hasWalletsPage ss
  logWrite "Finished initial page rendering"
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
      selectTrougth ss mname
    selectTrougth ss mname = do
      buildE <-getPostBuild
      case mname of
        Just name -> void $ nextWidget $ ffor buildE $ const $ Retractable {
                retractableNext = loadWalletPage name
              , retractablePrev = Just $ pure $ selectWalletsPage ss
              }
        Nothing -> selectWalletsPage ss
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
  let encryptedPrvStorage = view (authInfo'storage . storage'encryptedPrvStorage) authInfo
  case decryptPrvStorage encryptedPrvStorage pass of
    Left err -> pure $ Left err
    Right decryptedPrvStorage -> do
      let updatedPrvStorage = set prvStorage'currencyPrvStorages updatedPrvKeystore decryptedPrvStorage
      encryptPrvStorageResult <- encryptPrvStorage updatedPrvStorage pass
      case encryptPrvStorageResult of
        Left err -> pure $ Left err
        Right encryptedUpdatedPrvStorage -> pure $ Right $ set (authInfo'storage . storage'encryptedPrvStorage) encryptedUpdatedPrvStorage authInfo
      where
        currencyPrvStorages = view prvStorage'currencyPrvStorages decryptedPrvStorage
        currencyPubStorages = view (authInfo'storage . storage'pubStorage . pubStorage'currencyPubStorages) authInfo
        pubKeysNumber = M.fromList $ map counteKeys $ DM.toList currencyPubStorages
        counteKeys (currencyTxTag DM.:=> currencyPubStorage) = (currencyTxTagToCurrency currencyTxTag, (
             V.length $ pubKeystore'external (view currencyPubStorage'pubKeystore currencyPubStorage),
             V.length $ pubKeystore'internal (view currencyPubStorage'pubKeystore currencyPubStorage)
           ))
        updatedPrvKeystore =
          MM.merge
          MM.dropMissing
          MM.dropMissing
          (MM.zipWithMatched generateMissingPrvKeysHelper)
          currencyPrvStorages
          pubKeysNumber

generateMissingPrvKeysHelper ::
  Currency
  -> CurrencyPrvStorage -- ^ Private keystore
  -> (Int, Int)         -- ^ Total number of external and internal private keys respectively that should be stored in keystore
  -> CurrencyPrvStorage -- ^ Updated private keystore
generateMissingPrvKeysHelper _ (CurrencyPrvStorage prvKeystore) (goalExternalKeysNum, goalInternalKeysNum) =
  CurrencyPrvStorage $ PrvKeystore masterPrvKey updatedExternalPrvKeys updatedInternalPrvKeys
  where
    currentExternalKeys = prvKeystore'external prvKeystore
    currentInternalKeys = prvKeystore'internal prvKeystore
    masterPrvKey = prvKeystore'master prvKeystore
    extLength = V.length currentExternalKeys
    intLength = V.length currentInternalKeys
    updatedExternalPrvKeys = if extLength >= goalExternalKeysNum
      then currentExternalKeys
      else let
        l = goalExternalKeysNum - extLength
        v = V.unfoldrN l (\i -> Just (derivePrvKey masterPrvKey External (fromIntegral i), i+1)) extLength
        in currentExternalKeys V.++ v
    updatedInternalPrvKeys = if intLength >= goalInternalKeysNum
      then currentInternalKeys
      else let
        l = goalInternalKeysNum - intLength
        v = V.unfoldrN l (\i -> Just (derivePrvKey masterPrvKey Internal (fromIntegral i), i+1)) intLength
        in currentInternalKeys V.++ v

initialAuthedPage :: MonadFront t m => m ()
initialAuthedPage = wrapperSimple True $ divClass "main-page" $ do
  anon_name <- getWalletName
  h4 $ text $ "Congrats " <> anon_name <> "! You've made it!"
  logoutE <- row . outlineButton $ ("Logout" :: Text)
  void $ setAuthInfo $ Nothing <$ logoutE
