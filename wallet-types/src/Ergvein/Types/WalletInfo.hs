module Ergvein.Types.WalletInfo where

import Control.Lens
import Data.Text (Text)
import Ergvein.Crypto.ECIES
import Ergvein.Types.Storage

data WalletInfo = WalletInfo {
  _walletInfo'storage     :: !WalletStorage
, _walletInfo'eciesPubKey :: !ECIESPubKey
, _walletInfo'login       :: !Text
, _walletInfo'isUpdate    :: !Bool
  -- ^ This field indicates whether the widget should be redrawn in 'liftAuth'.
  -- 'False' means that the value obtained as a result of updating the previous 'WalletInfo',
  -- 'True' means that the value was newly created or loaded from the storage file at startup.
, _walletInfo'isPlain     :: !Bool
  -- ^ this field indicates if the storage is encrypted with empty string, aka not encrypted
} deriving (Eq)

makeLenses ''WalletInfo
