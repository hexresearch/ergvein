module Ergvein.Types.Address (
      BtcAddress
    , EgvAddress(..)
    , VLAddr(..)
    , btcAddrToText'
    , btcAddrToText
    , btcAddrFromText
    , BtcAddressType(..)
    , egvAddrToText
    , egvAddrFromText
    , egvAddrCurrency
  ) where

import Data.Aeson
import Data.Aeson.Types        (Parser)
import Data.ByteString.Short   (ShortByteString)
import Data.Hashable           (Hashable)
import Data.Serialize          (Serialize, put)
import Data.String             (IsString, fromString)
import Data.String.Conversions (cs)
import Data.Text               (Text)
import Ergvein.Crypto
import Ergvein.Types.Currency
import Ergvein.Types.Network
import GHC.Generics            (Generic)

import qualified Data.ByteString.Short     as BSS
import qualified Data.Serialize            as S
import qualified Data.Serialize.Get        as Get
import qualified Data.Serialize.Put        as Put
import qualified Network.Haskoin.Address   as HA
import qualified Text.Read                 as R

type BtcAddress = HA.Address

-- | Type for ERGO variable-length addresses.
newtype VLAddr = VLAddr { getErgVLAddr :: ShortByteString }
  deriving (Eq, Generic, Ord, Hashable)

instance Show VLAddr where
  showsPrec _ = shows . encodeHex . BSS.fromShort . getErgVLAddr

instance Read VLAddr where
  readPrec = do
    R.String str <- R.lexP
    maybe R.pfail return $ VLAddr . BSS.toShort <$> decodeHex (cs str)

instance IsString VLAddr where
  fromString str =
    case decodeHex $ cs str of
      Nothing -> error "Could not decode Ergo address from hex string"
      Just bs -> VLAddr (BSS.toShort bs)

instance Serialize VLAddr where
  put (VLAddr sbs) = Put.putShortByteString sbs
  get = VLAddr <$> (Get.remaining >>= Get.getShortByteString)

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
