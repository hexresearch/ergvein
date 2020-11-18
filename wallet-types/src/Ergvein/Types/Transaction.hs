module Ergvein.Types.Transaction (
      BtcTx
    , btcTxToString
    , btcTxFromString
    , ErgTx(..)
    , ergTxToString
    , ergTxFromString
    , EgvTx(..)
    , EgvTxMeta(..)
    , egvTxId
    , egvTxCurrency
    , TxId
    , BlockHeight
    , BlockHash
    , TxBlockIndex
    , MerkleSum
    , TxMerkleProof
    , TxFee
    , PubKeyScriptHash
    , TxHash(..)
    , hkTxHashToEgv
    , egvBlockHashToHk
    , TxOutIndex
    , currencyHeightStart
    , getEgvTxMeta
    , setEgvTxMeta
  ) where

import Control.Monad ((<=<))
import Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Control.DeepSeq
import Data.Either
import Data.Text as T
import Data.Time
import Data.Flat
import Data.Word
import Network.Haskoin.Crypto (getHash256)
import           Data.Hashable           (Hashable)
import           GHC.Generics            (Generic)
import qualified Data.ByteString.Short   as BSS
import           Text.Read               as R
import           Data.String.Conversions (cs)
import Ergvein.Aeson
import Ergvein.Crypto.Util
import Ergvein.Text
import Ergvein.Types.Currency

import           Data.Serialize              as S
import qualified Network.Haskoin.Block as HB
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
  toJSON = A.String . ergTxToString

data EgvTx
  = BtcTx { getBtcTx :: !BtcTx, getBtcTxMeta :: !(Maybe EgvTxMeta)}
  | ErgTx { getErgTx :: !ErgTx, getErgTxMeta :: !(Maybe EgvTxMeta)}
  deriving (Eq, Show, Read)

egvTxToString :: EgvTx -> Text
egvTxToString (BtcTx tx _) = btcTxToString tx
egvTxToString (ErgTx tx _) = ergTxToString tx

egvTxId :: EgvTx -> TxId
egvTxId (BtcTx tx _) = hkTxHashToEgv $ HK.txHash tx
egvTxId (ErgTx _  _) = error "egvTxId: implement for Ergo!"

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

-- | Hexadecimal representation of transaction hash
newtype TxHash = TxHash {getTxHash :: ShortByteString}
  deriving (Eq, Ord, Hashable, Generic, Flat, Serialize, NFData)

instance Show TxHash where
    showsPrec _ = shows . encodeHex . BSS.fromShort . getTxHash

instance Read TxHash where
    readPrec = do
        R.String str <- lexP
        maybe pfail return $ TxHash . BSS.toShort <$> decodeHex (cs str)

instance FromJSON TxHash where
  parseJSON = withText "TxHash" $
    either (fail "Failed to parse TxHash") (pure . TxHash . BSS.toShort) . hex2bsTE
  {-# INLINE parseJSON #-}

instance ToJSON TxHash where
  toJSON = A.String . bs2Hex . BSS.fromShort . getTxHash
  {-# INLINE toJSON #-}

instance FromJSON ShortByteString where
  parseJSON = withText "ShortByteString" $
    either (fail "Failed to parse a ShortByteString") (pure . BSS.toShort) . hex2bsTE
  {-# INLINE parseJSON #-}

instance ToJSON ShortByteString where
  toJSON = A.String . bs2Hex . BSS.fromShort
  {-# INLINE toJSON #-}

instance FromJSONKey TxHash where
instance ToJSONKey TxHash where

hkTxHashToEgv :: HK.TxHash -> TxHash
hkTxHashToEgv = TxHash . getHash256 . HK.getTxHash
{-# INLINE hkTxHashToEgv #-}

-- | Index of the UTXO
type TxOutIndex = Word

-- | index of the first block in blockchain
currencyHeightStart :: Currency -> BlockHeight
currencyHeightStart = \case BTC  -> 0
                            ERGO -> 1
{-# INLINE currencyHeightStart #-}

data EgvTxMeta = EgvTxMeta {
  etxMetaHeight :: !(Maybe BlockHeight)
, etxMetaHash   :: !(Maybe HB.BlockHash)
, etxMetaTime   :: !UTCTime
} deriving (Eq, Show, Read)

$(deriveJSON (aesonOptionsStripPrefix "etxMeta") ''EgvTxMeta)

egvBlockHashToHk :: BlockHash -> HB.BlockHash
egvBlockHashToHk bh = fromRight (error $ "Failed to convert bh: " <> show bh) $ runGet S.get (BSS.fromShort bh)
{-# INLINE egvBlockHashToHk #-}
