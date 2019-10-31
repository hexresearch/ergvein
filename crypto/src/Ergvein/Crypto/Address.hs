module Ergvein.Crypto.Address
  (
    EgvAddress(..)
  ) where

import Data.Aeson
import Ergvein.Crypto.Constants
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58

data EgvAddress = EgvAddress {
  egvAddrNetTag :: NetworkTag
, egvAddress    :: Address
} deriving (Eq, Ord, Show, Read)

instance ToJSON EgvAddress where
  toJSON (EgvAddress net key) = object [
      "tag"     .= toJSON net
    , "address" .= addrToJSON (getNetworkFromTag net) key
    ]

instance FromJSON EgvAddress where
  parseJSON = withObject "EgvAddress" $ \o -> do
    net    <- o .: "tag"
    key <- addrFromJSON (getNetworkFromTag net) =<< (o .: "pub_key")
    pure $ EgvAddress net key
