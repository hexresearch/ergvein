module Ergvein.Crypto.Address
  (
      EgvAddress(..)
    , EgvAddressContent(..)
    , addressToOutput
    , addressToScriptBS
    , egvAddrToString
  ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Ergvein.Types.Currency
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58 (encodeBase58)

type BtcAddress = Address

type ErgAddress = ByteString

data EgvAddressContent
  = BtcAddress { getBtcAddr :: BtcAddress }
  | ErgAddress { getErgAddr :: ErgAddress }
  deriving (Eq, Show, Read)

data EgvAddress = EgvAddress {
  egvAddrCur :: Currency
, egvAddress :: EgvAddressContent
} deriving (Eq, Show, Read)

egvAddrToString :: EgvAddressContent -> Text
egvAddrToString (BtcAddress addr) = addrToString (getCurrencyNetwork BTC) addr
egvAddrToString (ErgAddress addr) = encodeBase58 addr

egvAddrContentToJSON :: EgvAddressContent -> Value
egvAddrContentToJSON = String . egvAddrToString

-- addrFromJSON :: Network -> Value -> Parser Address
-- addrFromJSON net =
--     withText "address" $ \t ->
--         case stringToAddr net t of
--             Nothing -> fail "could not decode address"
--             Just x  -> return x

-- egvAddrContentFromJSON :: Network -> Value -> Parser EgvAddressContent
-- egvAddrContentFromJSON net = addrFromJSON (getCurrencyNetwork cur)

-- instance ToJSON EgvAddress where
--   toJSON (EgvAddress cur addr) = object [
--       "currency" .= toJSON cur
--     , "address"  .= egvAddrContentToJSON addr
--     ]

-- instance FromJSON EgvAddress where
--   parseJSON = withObject "EgvAddress" $ \o -> do
--     cur  <- o .: "currency"
--     addr <- egvAddrContentFromJSON (getCurrencyNetwork cur) =<< (o .: "address")
--     pure $ EgvAddress cur addr
