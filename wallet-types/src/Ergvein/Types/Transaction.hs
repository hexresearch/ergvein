module Ergvein.Types.Transaction (
      BlockHeight
    , BtcTx
    , BlockHash
    , TxBlockIndex
    , MerkleSum
    , TxMerkleProof
    , TxFee
    , PubKeyScriptHash
    , TxOutIndex
    , currencyHeightStart
    , egvBlockHashToHk
    , btcTxToString
    , btcTxFromString
    , ErgTx(..)
    , ergTxToString
    , ergTxFromString
    , EgvTx(..)
    , EgvTxMeta(..)
    , egvTxToString
    , getEgvTxMeta
    , setEgvTxMeta
    , egvTxCurrency
    , TxId
    , TxHash(..)
    , btcTxHashToStr
    , btcTxHashFromStr
    , ergTxHashToStr
    , ergTxHashFromStr
    , egvTxHashToStr
    , egvTxHashFromStr
    , hkTxHashToEgv
    , egvTxId
  ) where

import Control.DeepSeq
import Control.Monad ((<=<))
import Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Either
import Data.Hashable (Hashable)
import Data.Serialize as S
import Data.Text as T
import Data.Time
import Data.Word
import Ergvein.Aeson
import Ergvein.Crypto.Util
import Ergvein.Types.Currency
import GHC.Generics (Generic)

import qualified Data.ByteString.Short       as BSS
import qualified Network.Haskoin.Block       as HB
import qualified Network.Haskoin.Transaction as HK

-- | Number of blocks before current one, from the starting from Genesis block with height of zero
type BlockHeight = Word64

-- | Hash of block (usually header only) that identifies block.
type BlockHash = ShortByteString

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

egvBlockHashToHk :: BlockHash -> HB.BlockHash
egvBlockHashToHk bh = fromRight (error $ "Failed to convert bh: " <> show bh) $ runGet S.get (BSS.fromShort bh)
{-# INLINE egvBlockHashToHk #-}

-- | Index of the UTXO
type TxOutIndex = Word

-- | index of the first block in blockchain
currencyHeightStart :: Currency -> BlockHeight
currencyHeightStart = \case BTC  -> 0
                            ERGO -> 1
{-# INLINE currencyHeightStart #-}

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
  toJSON = A.String . ergTxToString

data EgvTxMeta = EgvTxMeta {
  etxMetaHeight :: !(Maybe BlockHeight)
, etxMetaHash   :: !(Maybe HB.BlockHash)
, etxMetaTime   :: !UTCTime
} deriving (Eq, Show, Read)

$(deriveJSON (aesonOptionsStripPrefix "etxMeta") ''EgvTxMeta)

data EgvTx
  = BtcTx { getBtcTx :: !BtcTx, getBtcTxMeta :: !(Maybe EgvTxMeta)}
  | ErgTx { getErgTx :: !ErgTx, getErgTxMeta :: !(Maybe EgvTxMeta)}
  deriving (Eq, Show, Read)

egvTxToString :: EgvTx -> Text
egvTxToString (BtcTx tx _) = btcTxToString tx
egvTxToString (ErgTx tx _) = ergTxToString tx

egvTxCurrency :: EgvTx -> Currency
egvTxCurrency e = case e of
  BtcTx{} -> BTC
  ErgTx{} -> ERGO

egvTxFromJSON :: Currency -> Value -> Parser EgvTx
egvTxFromJSON = \case
  BTC -> withText "Bitcoin transaction" $ \t ->
    case btcTxFromString t of
      Nothing -> fail "could not decode Bitcoin transaction"
      Just x  -> return $ BtcTx x Nothing
  ERGO -> withText "Ergo transaction" $ \t ->
    case ergTxFromString t of
      Nothing -> fail "could not decode Ergo transaction"
      Just x  -> return $ ErgTx x Nothing

getEgvTxMeta :: EgvTx -> Maybe EgvTxMeta
getEgvTxMeta etx= case etx of
  BtcTx _ m -> m
  ErgTx _ m -> m

setEgvTxMeta :: EgvTx -> Maybe EgvTxMeta -> EgvTx
setEgvTxMeta etx mh = case etx of
  BtcTx tx _ -> BtcTx tx mh
  ErgTx tx _ -> ErgTx tx mh

instance ToJSON EgvTx where
  toJSON (BtcTx tx meta) = object [
      "currency"  .= toJSON BTC
    , "tx"        .= btcTxToString tx
    , "meta"      .= toJSON meta
    ]
  toJSON (ErgTx tx meta) = object [
      "currency"  .= toJSON ERGO
    , "tx"        .= ergTxToString tx
    , "meta"      .= toJSON meta
    ]

instance FromJSON EgvTx where
  parseJSON = withObject "EgvTx" $ \o -> do
    cur  <- o .: "currency"
    meta <- o .:? "meta"
    tx   <- egvTxFromJSON cur =<< (o .: "tx")
    pure $ setEgvTxMeta tx meta

-- | Internal representation of transaction id
type TxId = TxHash

-- | Hash of transaction
data TxHash
  = BtcTxHash {getBtcTxHash :: HK.TxHash}
  | ErgTxHash {getErgTxHash :: ShortByteString}
  deriving (Eq, Show, Read, Ord, Hashable, Generic, Serialize, NFData)

btcTxHashToStr :: HK.TxHash -> Text
btcTxHashToStr = HK.txHashToHex

ergTxHashToStr :: ShortByteString -> Text
ergTxHashToStr = encodeHex . BSS.fromShort

egvTxHashToStr :: TxHash -> Text
egvTxHashToStr (BtcTxHash h) = btcTxHashToStr h
egvTxHashToStr (ErgTxHash h) = ergTxHashToStr h

btcTxHashFromStr :: Text -> Maybe HK.TxHash
btcTxHashFromStr = HK.hexToTxHash

ergTxHashFromStr :: Text -> Maybe ShortByteString
ergTxHashFromStr t = BSS.toShort <$> decodeHex t

egvTxHashFromStr :: Currency -> Text -> Maybe TxHash
egvTxHashFromStr BTC  addr = BtcTxHash <$> btcTxHashFromStr addr
egvTxHashFromStr ERGO addr = ErgTxHash <$> ergTxHashFromStr addr

egvTxHashToJSON :: TxHash -> Value
egvTxHashToJSON = A.String . egvTxHashToStr

egvTxHashFromJSON :: Currency -> Value -> Parser TxHash
egvTxHashFromJSON = \case
  BTC -> withText "txHash" $ \t ->
    case btcTxHashFromStr t of
      Nothing -> fail "could not decode transaction hash"
      Just x  -> return $ BtcTxHash x
  ERGO -> withText "txHash" $ \t ->
    case ergTxHashFromStr t of
      Nothing -> fail "could not decode transaction hash"
      Just x  -> return $ ErgTxHash x

instance ToJSON TxHash where
  toJSON egvTxHash@(BtcTxHash _) = object [
      "currency" .= toJSON BTC
    , "txHash"   .= egvTxHashToJSON egvTxHash
    ]
  toJSON egvTxHash@(ErgTxHash _) = object [
      "currency" .= toJSON ERGO
    , "txHash"   .= egvTxHashToJSON egvTxHash
    ]

instance FromJSON TxHash where
  parseJSON = withObject "TxHash" $ \o -> do
    cur  <- o .: "currency"
    egvTxHashFromJSON cur =<< (o .: "txHash")

instance FromJSONKey TxHash where
instance ToJSONKey TxHash where

hkTxHashToEgv :: HK.TxHash -> TxHash
hkTxHashToEgv = BtcTxHash
{-# INLINE hkTxHashToEgv #-}

egvTxId :: EgvTx -> TxId
egvTxId (BtcTx tx _) = hkTxHashToEgv $ HK.txHash tx
egvTxId (ErgTx _ _)  = error "egvTxId: implement for Ergo!"
