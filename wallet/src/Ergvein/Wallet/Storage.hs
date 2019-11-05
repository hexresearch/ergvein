module Ergvein.Wallet.Storage
  (
    ErgveinStorage(..)
  , StorageAlerts(..)
  , loadStorageFromFile
  ) where

import Control.Monad.IO.Class
import Data.Text(Text)
import Ergvein.Aeson
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Native
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Secure.Data
import Network.Haskoin.Address
import Network.Haskoin.Keys
import qualified Data.Map.Strict as M
import qualified Data.Text as T


type Password = Text

data ErgveinStorage = ErgveinStorage{
  storage'wallet     :: EncryptedWalletData
, storage'pubKeys    :: M.Map EgvXPubKey [Base58]
, storage'walletName :: Text
}

instance Eq ErgveinStorage where
  a == b = storage'walletName a == storage'walletName b

$(deriveJSON aesonOptionsStripToApostroph ''ErgveinStorage)

storageFileName :: Text
storageFileName = "storage"

-- TODO: Actually decrypt the storage
decryptStorage :: MonadIO m => Password -> Text -> m (Either StorageAlerts ErgveinStorage)
decryptStorage pass txt = pure $ either (Left . SADecodeError) Right $ text2json txt

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives) => Password -> m (Either StorageAlerts ErgveinStorage)
loadStorageFromFile pass = do
  storageResp <- readStoredFile storageFileName
  either (pure . Left . SANativeAlert) (decryptStorage pass . T.concat) storageResp

-- Alerts regarding secure storage system
data StorageAlerts
  = SADecodeError Text
  | SALoadedSucc
  | SANativeAlert NativeAlerts
  deriving (Eq)

instance LocalizedPrint StorageAlerts where
  localizedShow l v = case l of
    English -> case v of
      SADecodeError e -> "Storage loading error: " <> e
      SALoadedSucc    -> "Storage loaded"
      SANativeAlert a -> localizedShow l a
    Russian -> localizedShow English v
