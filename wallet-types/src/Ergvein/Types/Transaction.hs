module Ergvein.Types.Transaction (
      BlockHeight
    , BlockHash
    , TxBlockIndex
    , MerkleSum
    , TxMerkleProof
    , TxFee
    , PubKeyScriptHash
    , TxOutIndex
    , RbfEnabled
    , currencyHeightStart
    , egvBlockHashToHk
    , EgvTx(..)
    , toTxBtc
    , toTxErg
    , egvTxToString
    , egvTxCurrency
    , getEgvTxMeta
    , setEgvTxMeta
    , TxId
    , ErgTxId(..)
    , TxHash(..)
    , toBtcTxHash
    , toErgTxHash
    , egvTxHashToStr
    , egvTxHashFromStr
    , hkTxHashToEgv
    , egvTxId
    , egvTxsToBtc
    , egvTxsToErg
    , module Reexport
  ) where

import Control.DeepSeq
import Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.ByteString.Short (ShortByteString)
import Data.Either
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Maybe
import Data.SafeCopy
import Data.Serialize (put, get)
import Data.Serialize as S
import Data.Text as T
import GHC.Generics (Generic)

import Ergvein.Types.Currency
import Ergvein.Types.Height
import Ergvein.Types.Orphanage ()
import Ergvein.Types.Transaction.Btc as Reexport
import Ergvein.Types.Transaction.Ergo as Reexport
import Ergvein.Types.Transaction.Meta as Reexport

import qualified Data.ByteString.Short       as BSS
import qualified Data.Map.Strict             as M
import qualified Network.Haskoin.Block       as HB
import qualified Network.Haskoin.Transaction as HK

-- | Hash of block (usually header only) that identifies block.
type BlockHash = ShortByteString

-- | Index of the transaction in block
type TxBlockIndex = Word

-- | Node in Merkle tree, hash of concatenated child nodes
type MerkleSum = Text

-- | Brunch of MerkleSums in Merkle tree, for transaction validation, deepest MerkleSum first
type TxMerkleProof = [MerkleSum]

-- | Fee included with transaction as price for processing transaction by miner
type TxFee = MoneyAmount

-- | SHA256 hash of locking script with big-endian byte order, used to track transfers due inaccessibility
-- of transaction addresses when indexer scans blockchain
type PubKeyScriptHash = Text

type RbfEnabled = Bool

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

data EgvTx
  = TxBtc !BtcTx
  | TxErg !ErgTx
  deriving (Eq, Show, Read)

instance SafeCopy EgvTx where
  putCopy v = contain $ case v of
    TxBtc tx -> put BTC >> safePut tx
    TxErg tx -> put ERGO >> safePut tx
  getCopy = contain $ do
    c <- get
    case c of
      BTC -> TxBtc <$> safeGet
      ERGO -> TxErg <$> safeGet

toTxBtc :: EgvTx -> Maybe BtcTx
toTxBtc = \case
  TxBtc t -> Just t
  _ -> Nothing

toTxErg :: EgvTx -> Maybe ErgTx
toTxErg = \case
  TxErg t -> Just t
  _ -> Nothing

egvTxToString :: EgvTx -> Text
egvTxToString (TxBtc (BtcTx tx _)) = btcTxToString tx
egvTxToString (TxErg (ErgTx tx _)) = ergTxToString tx

egvTxCurrency :: EgvTx -> Currency
egvTxCurrency e = case e of
  TxBtc{} -> BTC
  TxErg{} -> ERGO

egvTxFromJSON :: Currency -> Value -> Parser EgvTx
egvTxFromJSON = \case
  BTC -> withText "Bitcoin transaction" $ \t ->
    case btcTxFromString t of
      Nothing -> fail "could not decode Bitcoin transaction"
      Just x  -> return $ TxBtc $ BtcTx x Nothing
  ERGO -> withText "Ergo transaction" $ \t ->
    case ergTxFromString t of
      Nothing -> fail "could not decode Ergo transaction"
      Just x  -> return $ TxErg $ ErgTx x Nothing

instance ToJSON EgvTx where
  toJSON (TxBtc (BtcTx tx meta)) = object [
      "currency"  .= toJSON BTC
    , "tx"        .= btcTxToString tx
    , "meta"      .= toJSON meta
    ]
  toJSON (TxErg (ErgTx tx meta)) = object [
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

getEgvTxMeta :: EgvTx -> Maybe EgvTxMeta
getEgvTxMeta etx= case etx of
  TxBtc (BtcTx _ m) -> m
  TxErg (ErgTx _ m) -> m

setEgvTxMeta :: EgvTx -> Maybe EgvTxMeta -> EgvTx
setEgvTxMeta etx mh = case etx of
  TxBtc (BtcTx tx _) -> TxBtc (BtcTx tx mh)
  TxErg (ErgTx tx _) -> TxErg (ErgTx tx mh)

-- | Hash of transaction
data TxHash
  = BtcTxHash {getBtcTxHash :: !BtcTxId}
  | ErgTxHash {getErgTxHash :: !ErgTxId}
  deriving (Eq, Show, Read, Ord, Hashable, Serialize, Generic, NFData)

instance SafeCopy TxHash where
  putCopy = contain . put
  getCopy = contain get

toBtcTxHash :: TxHash -> Maybe BtcTxId
toBtcTxHash = \case
  BtcTxHash t -> Just t
  _ -> Nothing

toErgTxHash :: TxHash -> Maybe ErgTxId
toErgTxHash = \case
  ErgTxHash t -> Just t
  _ -> Nothing

egvTxHashToStr :: TxHash -> Text
egvTxHashToStr (BtcTxHash h) = btcTxHashToStr h
egvTxHashToStr (ErgTxHash h) = ergTxHashToStr h

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
egvTxId (TxBtc (BtcTx tx _)) = hkTxHashToEgv $ HK.txHash tx
egvTxId (TxErg (ErgTx _ _))  = error "egvTxId: implement for Ergo!"

egvTxsToBtc :: Map TxId EgvTx -> Map BtcTxId BtcTxRaw
egvTxsToBtc = M.mapKeys (fromMaybe (error "impossible: btcTxs") . toBtcTxHash) . M.mapMaybe (fmap getBtcTx . toTxBtc)

egvTxsToErg :: Map TxId EgvTx -> Map ErgTxId ErgTxRaw
egvTxsToErg = M.mapKeys (fromMaybe (error "impossible: ergoTxs") . toErgTxHash) . M.mapMaybe (fmap getErgTx . toTxErg)
