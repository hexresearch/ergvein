module Ergvein.Types.Address
  (
      BtcAddress(..)
    , ErgAddress(..)
    , EgvAddress(..)
    , egvAddrToString
    , stringToEgvAddr
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text, pack)
import Ergvein.Types.Currency
import Network.Haskoin.Address.Base58 (encodeBase58, decodeBase58)

import qualified Data.ByteString.Short as BSS
import qualified Network.Haskoin.Address as HA

type BtcAddress = HA.Address

data ErgAddress
  = ErgPubKeyAddress     { getAddr :: !ShortByteString }
  | ErgScriptHashAddress { getAddr :: !ShortByteString }
  | ErgScriptAddress     { getAddr :: !ShortByteString }
  deriving (Eq, Show, Read)

data EgvAddress
  = BtcAddress { getBtcAddr :: BtcAddress }
  | ErgAddress { getErgAddr :: ErgAddress }
  deriving (Eq, Show, Read)

btcAddrToString :: BtcAddress -> Text
btcAddrToString = HA.addrToString (getCurrencyNetwork BTC)

-- TODO: fix this
ergAddrToString :: ErgAddress -> Text
ergAddrToString = encodeBase58 . BSS.fromShort . getAddr

egvAddrToString :: EgvAddress -> Text
egvAddrToString (BtcAddress addr) = btcAddrToString addr
egvAddrToString (ErgAddress addr) = ergAddrToString addr

stringToBtcAddr :: Text -> Maybe BtcAddress
stringToBtcAddr = HA.stringToAddr (getCurrencyNetwork BTC)

-- TODO: fix this
stringToErgAddr :: Text -> Maybe ErgAddress
stringToErgAddr t =  ErgPubKeyAddress . BSS.toShort <$> decodeBase58 t

stringToEgvAddr :: Currency -> Text -> Maybe EgvAddress
stringToEgvAddr BTC  addr = BtcAddress <$> stringToBtcAddr addr
stringToEgvAddr ERGO addr = ErgAddress <$> stringToErgAddr addr

egvAddrToJSON :: EgvAddress -> Value
egvAddrToJSON = String . egvAddrToString

egvAddrFromJSON :: Currency -> Value -> Parser EgvAddress
egvAddrFromJSON cur
  | cur == BTC = withText "address" $ \t ->
    case stringToBtcAddr t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ BtcAddress x
  | cur == ERGO = withText "address" $ \t ->
    case stringToErgAddr t of
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
