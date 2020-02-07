module Ergvein.Types.Address
  (
      EgvAddress(..)
    , EgvAddressContent(..)
    , addressToOutput
    , addressToScriptBS
    , egvAddrContentToString
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Ergvein.Types.Currency
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58 (encodeBase58, decodeBase58)

type BtcAddress = Address

type ErgAddress = ByteString

data EgvAddressContent
  = BtcAddress { getBtcAddr :: BtcAddress }
  | ErgAddress { getErgAddr :: ErgAddress }
  deriving (Eq, Show, Read)

data EgvAddress = EgvAddress {
    egvAddrCurrency :: Currency
  , egvAddrContent :: EgvAddressContent
  } deriving (Eq, Show, Read)

egvAddrContentToString :: EgvAddressContent -> Text
egvAddrContentToString (BtcAddress addr) = addrToString (getCurrencyNetwork BTC) addr
egvAddrContentToString (ErgAddress addr) = encodeBase58 addr

egvAddrContentToJSON :: EgvAddressContent -> Value
egvAddrContentToJSON = String . egvAddrContentToString

egvAddrContentFromJSON :: Currency -> Value -> Parser EgvAddressContent
egvAddrContentFromJSON cur
  | cur == BTC = withText "address" $ \t ->
    case stringToAddr (getCurrencyNetwork BTC) t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ BtcAddress x
  | cur == ERGO = withText "address" $ \t ->
    case decodeBase58 t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ ErgAddress x

instance ToJSON EgvAddress where
  toJSON (EgvAddress cur addr) = object [
      "currency" .= toJSON cur
    , "address"  .= egvAddrContentToJSON addr
    ]

instance FromJSON EgvAddress where
  parseJSON = withObject "EgvAddress" $ \o -> do
    cur  <- o .: "currency"
    addr <- egvAddrContentFromJSON cur =<< (o .: "address")
    pure $ EgvAddress cur addr
