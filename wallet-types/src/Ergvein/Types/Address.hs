module Ergvein.Types.Address
  (
      EgvAddress(..)
    , egvAddrToString
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

data EgvAddress
  = BtcAddress { getBtcAddr :: BtcAddress }
  | ErgAddress { getErgAddr :: ErgAddress }
  deriving (Eq, Show, Read)

egvAddrToString :: EgvAddress -> Text
egvAddrToString (BtcAddress addr) = addrToString (getCurrencyNetwork BTC) addr
egvAddrToString (ErgAddress addr) = encodeBase58 addr

egvAddrToJSON :: EgvAddress -> Value
egvAddrToJSON = String . egvAddrToString

egvAddrFromJSON :: Currency -> Value -> Parser EgvAddress
egvAddrFromJSON cur
  | cur == BTC = withText "address" $ \t ->
    case stringToAddr (getCurrencyNetwork BTC) t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ BtcAddress x
  | cur == ERGO = withText "address" $ \t ->
    case decodeBase58 t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ ErgAddress x

instance ToJSON EgvAddress where
  toJSON egvAddr@(BtcAddress addr) = object [
      "currency" .= toJSON BTC
    , "address"  .= egvAddrToJSON egvAddr
    ]
  toJSON egvAddr@(ErgAddress addr) = object [
      "currency" .= toJSON ERGO
    , "address"  .= egvAddrToJSON egvAddr
    ]

instance FromJSON EgvAddress where
  parseJSON = withObject "EgvAddress" $ \o -> do
    cur  <- o .: "currency"
    egvAddrFromJSON cur =<< (o .: "address")
