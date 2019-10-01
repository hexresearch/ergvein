module Ergvein.Wallet.Settings where

import Control.Lens
import Data.Default
import Ergvein.Aeson
import Ergvein.Lens

data Settings = Settings {

} deriving (Eq, Show)

instance Default Settings where
  def = Settings

$(deriveJSON (aesonOptionsStripPrefix "settings") ''Settings)

makeLensesWith humbleFields ''Settings
