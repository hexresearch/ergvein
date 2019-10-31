module Ergvein.Wallet.Storage
  (
    ErgveinStorage(..)
  , loadStorageFromFile
  ) where

import Control.Monad.IO.Class
import Data.Text(Text)
import Ergvein.Aeson
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Wallet.Native
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

decryptStorage :: MonadIO m => Password -> Text -> m (Either Text ErgveinStorage)
decryptStorage pass txt = pure $ text2json txt

loadStorageFromFile :: (MonadIO m, HasStoreDir m, PlatformNatives) => Password -> m (Either Text ErgveinStorage)
loadStorageFromFile pass = do
  storageRawLines <- readStoredFile storageFileName
  case storageRawLines of
    [] -> do
      appendStoredFile storageFileName ""
      pure $ Left $ "Storage file is empty"
    _ -> decryptStorage pass $ T.concat storageRawLines
