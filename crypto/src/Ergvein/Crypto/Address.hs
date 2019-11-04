module Ergvein.Crypto.Address
  (
    EgvAddress(..)
  ) where

import Data.Aeson
import Ergvein.Crypto.Constants
import Ergvein.Types.Currency
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58

data EgvAddress = EgvAddress {
  egvAddrCur :: Currency
, egvAddress :: Address
} deriving (Eq, Ord, Show, Read)

instance ToJSON EgvAddress where
  toJSON (EgvAddress cur key) = object [
      "cur"     .= toJSON cur
    , "address" .= addrToJSON (getCurrencyNetwork cur) key
    ]

instance FromJSON EgvAddress where
  parseJSON = withObject "EgvAddress" $ \o -> do
    cur    <- o .: "cur"
    key <- addrFromJSON (getCurrencyNetwork cur) =<< (o .: "pub_key")
    pure $ EgvAddress cur key
