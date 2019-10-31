module Ergvein.Wallet.Settings where

import Control.Lens
import Data.Default
import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Wallet.Language

data Settings = Settings {
  settingsLang :: Language
} deriving (Eq, Show)

instance Default Settings where
  def = Settings English

$(deriveJSON (aesonOptionsStripPrefix "settings") ''Settings)

makeLensesWith humbleFields ''Settings
