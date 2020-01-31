module Ergvein.Types.AuthInfo (
  AuthInfo(..)
) where

import Control.Lens
import Ergvein.Crypto.ECIES
import Ergvein.Lens
import Ergvein.Types.Storage

data AuthInfo = AuthInfo {
  authInfo'storage     :: ErgveinStorage
, authInfo'eciesPubKey :: ECIESPubKey
, authInfo'isUpdate    :: Bool
  -- ^ This field indicates whether the widget should be redrawn in 'liftAuth'.
  -- 'False' means that the value obtained as a result of updating the previous 'AuthInfo',
  -- 'True' means that the value was newly created or loaded from the storage file at startup.
} deriving (Eq)

-- makeLensesWith humbleFields ''AuthInfo
