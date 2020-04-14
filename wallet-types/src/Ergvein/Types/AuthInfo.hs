module Ergvein.Types.AuthInfo where

import Control.Lens
import Data.Text (Text)
import Ergvein.Crypto.ECIES
import Ergvein.Types.Storage

data AuthInfo = AuthInfo {
  _authInfo'storage     :: WalletStorage
, _authInfo'eciesPubKey :: ECIESPubKey
, _authInfo'login       :: Text
, _authInfo'isUpdate    :: Bool
  -- ^ This field indicates whether the widget should be redrawn in 'liftAuth'.
  -- 'False' means that the value obtained as a result of updating the previous 'AuthInfo',
  -- 'True' means that the value was newly created or loaded from the storage file at startup.
} deriving (Eq)

makeLenses ''AuthInfo
