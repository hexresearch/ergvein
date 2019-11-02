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
import Ergvein.Wallet.Native
import Ergvein.Wallet.Language
import Ergvein.Wallet.Storage.Secure.Data
import Network.Haskoin.Address
import Network.Haskoin.Keys
import qualified Data.Map.Strict as M
import qualified Data.Text as T


type Password = Text

data ErgveinStorage = ErgveinStorage{
  storageWallet   :: EncryptedWalletData
, storagePubKeys  :: M.Map EgvXPubKey [Base58]
}

$(deriveJSON defaultOptions ''ErgveinStorage)

storageFileName :: Text
storageFileName = "storage"

-- TODO: Actually decrypt the storage
decryptStorage :: MonadIO m => Password -> Text -> m (Either StorageAlerts ErgveinStorage)
decryptStorage pass txt = pure $ either (Left . SADecodeError) Right $ text2json txt

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives) => Password -> m (Either StorageAlerts ErgveinStorage)
loadStorageFromFile pass = do
  storageRawLines <- readStoredFile storageFileName
  case storageRawLines of
    [] -> do
      appendStoredFile storageFileName ""
      pure $ Left SAEmptyError
    _ -> decryptStorage pass $ T.concat storageRawLines

-- Alerts regarding secure storage system
data StorageAlerts
  = SADecodeError Text
  | SALoadedSucc
  | SAEmptyError
  deriving (Eq)

instance LocalizedPrint StorageAlerts where
  localizedShow l v = case l of
    English -> case v of
      SADecodeError e -> "Storage loading error: " <> e
      SALoadedSucc    -> "Storage loaded"
      SAEmptyError    -> "Error! Storage is empty"
    Russian -> localizedShow English v
