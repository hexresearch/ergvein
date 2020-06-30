module Ergvein.Types.Transaction (
      BtcTx(..)
    , btcTxToString
    , btcTxFromString
    , ErgTx(..)
    , ergTxToString
    , ergTxFromString
    , EgvTx(..)
    , egvTxId
    , TxId
    , TxHexView
    , BlockHeight
    , BlockHash
    , TxBlockIndex
    , MerkleSum
    , TxMerkleProof
    , TxFee
    , PubKeyScriptHash
    , TxHash
    , TxOutIndex
    , currencyHeightStart
  ) where

import Control.Monad (mzero, (<=<))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Text
import Data.Word
import Ergvein.Text
import Ergvein.Crypto.Util
import Ergvein.Types.Currency
import Data.String (IsString, fromString)

import           Data.Serialize              as S
import qualified Network.Haskoin.Transaction as HK

type BtcTx = HK.Tx

btcTxToString :: BtcTx -> Text
btcTxToString = encodeHex . S.encode

btcTxFromString :: Text -> Maybe BtcTx
btcTxFromString = eitherToMaybe . S.decode <=< decodeHex

newtype ErgTx = ErgTransaction ByteString
  deriving (Eq, Show, Read)

ergTxToString :: ErgTx -> Text
ergTxToString (ErgTransaction tx) = encodeHex tx

ergTxFromString :: Text -> Maybe ErgTx
ergTxFromString t = ErgTransaction <$> decodeHex t

instance FromJSON ErgTx where
  parseJSON = withText "ErgTx" $ \t ->
    case ergTxFromString t of
      Nothing -> fail "could not decode ERGO transaction"
      Just x  -> return x

instance ToJSON ErgTx where
  toJSON = String . ergTxToString

data EgvTx
  = BtcTx { getBtcTx :: !BtcTx, getBtcHeight :: !(Maybe BlockHeight) }
  | ErgTx { getErgTx :: !ErgTx, getErgHeight :: !(Maybe BlockHeight) }
  deriving (Eq, Show, Read)

egvTxToString :: EgvTx -> Text
egvTxToString (BtcTx tx _) = btcTxToString tx
egvTxToString (ErgTx tx _) = ergTxToString tx

egvTxId :: EgvTx -> TxId
egvTxId (BtcTx tx _) = HK.txHashToHex $ HK.txHash tx
egvTxId (ErgTx tx _) = error "egvTxId: implement for Ergo!"

egvTxToJSON :: EgvTx -> Value
egvTxToJSON = String . egvTxToString

egvTxFromJSON :: Currency -> Value -> Parser EgvTx
egvTxFromJSON cur
  | cur == BTC = withText "Bitcoin transaction" $ \t ->
    case btcTxFromString t of
      Nothing -> fail "could not decode Bitcoin transaction"
      Just x  -> return $ BtcTx x Nothing
  | cur == ERGO = withText "Ergo transaction" $ \t ->
    case ergTxFromString t of
      Nothing -> fail "could not decode Ergo transaction"
      Just x  -> return $ ErgTx x Nothing

setEgvTxHeight :: EgvTx -> Maybe BlockHeight -> EgvTx
setEgvTxHeight etx mh = case etx of
  BtcTx tx _ -> BtcTx tx mh
  ErgTx tx _ -> ErgTx tx mh

instance ToJSON EgvTx where
  toJSON egvTx@(BtcTx tx h) = object [
      "currency"  .= toJSON BTC
    , "tx"        .= btcTxToString tx
    , "height"    .= toJSON h
    ]
  toJSON egvTx@(ErgTx tx h) = object [
      "currency"  .= toJSON ERGO
    , "tx"        .= ergTxToString tx
    , "height"    .= toJSON h
    ]

instance FromJSON EgvTx where
  parseJSON = withObject "EgvTx" $ \o -> do
    cur  <- o .: "currency"
    h    <- o .:? "height"
    tx   <- egvTxFromJSON cur =<< (o .: "tx")
    pure $ setEgvTxHeight tx h

-- | Hexadecimal representation of transaction id
type TxId = Text

-- | Hexadecimal representation of transaction
type TxHexView = Text

-- | Number of blocks before current one, from the starting from Genesis block with height of zero
type BlockHeight = Word64

-- | Hash of block (usually header only) that identifies block.
type BlockHash = Text

-- | Index of the transaction in block
type TxBlockIndex = Word

-- | Node in Merkle tree, hash of concatenated child nodes
type MerkleSum = Text

-- | Brunch of MerkleSums in Merkle tree, for transaction validation, deepest MerkleSum first
type TxMerkleProof = [MerkleSum]

-- | Fee included with transaction as price for processing transaction by miner
type TxFee = MoneyUnit

-- | SHA256 hash of locking script with big-endian byte order, used to track transfers due inaccessibility
-- of transaction addresses when indexer scans blockchain
type PubKeyScriptHash = Text

-- | Hexadecimal representation of transaction hash
type TxHash = Text

-- | Index of the UTXO
type TxOutIndex = Word

-- | index of the first block in blockchain
currencyHeightStart :: Currency -> BlockHeight
currencyHeightStart = \case BTC  -> 0
                            ERGO -> 1
