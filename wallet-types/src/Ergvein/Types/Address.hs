module Ergvein.Types.Address (
      BtcAddress
    , EgvAddress(..)
    , btcAddrToText'
    , btcAddrToText
    , btcAddrFromText
    , BtcAddressType(..)
    , btcAddrToBtcOutType
    , egvAddrToText
    , egvAddrFromText
    , egvAddrCurrency
    , btcAddrToBtcOutType
  ) where

import Data.Aeson
import Data.Aeson.Types        (Parser)
import Data.Serialize          (Serialize)
import Data.Text               (Text)
import Ergvein.Types.Currency
import Ergvein.Types.Network
import GHC.Generics            (Generic)

import qualified Network.Haskoin.Address   as HA

type BtcAddress = HA.Address

data EgvAddress
  = BtcAddress { getBtcAddr :: !BtcAddress }
  deriving (Eq, Generic, Show, Read, Serialize)

btcAddrToText' :: BtcNetwork -> BtcAddress -> Text
btcAddrToText' net addr = case HA.addrToString net addr of
  Nothing -> undefined -- FIXME
  Just s -> s

btcAddrToText :: BtcAddress -> Text
btcAddrToText = btcAddrToText' net
  where net = getBtcNetwork $ getCurrencyNetwork BTC

egvAddrCurrency :: EgvAddress -> Currency
egvAddrCurrency addr = case addr of
  BtcAddress{} -> BTC

egvAddrToText :: EgvAddress -> Text
egvAddrToText (BtcAddress addr) = btcAddrToText addr

btcAddrFromText :: Text -> Maybe BtcAddress
btcAddrFromText = HA.stringToAddr net
  where net = getBtcNetwork $ getCurrencyNetwork BTC

egvAddrFromText :: Currency -> Text -> Maybe EgvAddress
egvAddrFromText BTC  addr = BtcAddress <$> btcAddrFromText addr

egvAddrToJSON :: EgvAddress -> Value
egvAddrToJSON = String . egvAddrToText

egvAddrFromJSON :: Currency -> Value -> Parser EgvAddress
egvAddrFromJSON = \case
  BTC -> withText "address" $ \t ->
    case btcAddrFromText t of
      Nothing -> fail "could not decode address"
      Just x  -> return $ BtcAddress x

instance ToJSON EgvAddress where
  toJSON egvAddr@(BtcAddress _) = object [
      "currency" .= toJSON BTC
    , "address"  .= egvAddrToJSON egvAddr
    ]

instance FromJSON EgvAddress where
  parseJSON = withObject "EgvAddress" $ \o -> do
    cur  <- o .: "currency"
    egvAddrFromJSON cur =<< (o .: "address")

data BtcAddressType =
    BtcP2PK
  | BtcP2PKH
  | BtcP2MS
  | BtcP2SH
  | BtcP2WPKH
  | BtcP2WSH
  | BtcDataCarrier
  deriving (Read, Show, Eq)

btcAddrToBtcOutType :: BtcAddress -> BtcAddressType
btcAddrToBtcOutType = \case
  HA.PubKeyAddress _ -> BtcP2PKH
  HA.ScriptAddress _ -> BtcP2SH
  HA.WitnessPubKeyAddress _ -> BtcP2WPKH
  HA.WitnessScriptAddress _ -> BtcP2WSH
