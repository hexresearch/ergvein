module Ergvein.Wallet.Settings where

import Control.Lens
import Data.Default
import Data.Text(Text)
import Ergvein.Aeson
import Ergvein.Lens
import Ergvein.Wallet.Language

data Settings = Settings {
  settingsLang      :: Language
, settingsStoreDir  :: Text
} deriving (Eq, Show)

-- | TODO: Implement sensible defaults
instance Default Settings where
  def = Settings English "."

$(deriveJSON (aesonOptionsStripPrefix "settings") ''Settings)

makeLensesWith humbleFields ''Settings
