-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc
  ( -- * SegWit address
    SegWitAddress(..)
  , guardSegWit
  , fromSegWit
    -- * Filter
  , BtcAddrFilter(..)
  , encodeBtcAddrFilter
  , decodeBtcAddrFilter
  , makeBtcFilter
  , applyBtcFilter
  )
where

import           Data.ByteArray.Hash            ( SipKey(..) )
import           Data.ByteString                ( ByteString )
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.Serialize                 ( encode )
import           Data.Word
import           Ergvein.Filters.GCS
import           GHC.Generics
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Crypto         ( Hash160
                                                , Hash256
                                                )
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction

import qualified Data.Attoparsec.Binary        as A
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Strict               as M

-- | Special wrapper around SegWit address (P2WPKH or P2WSH) to distinct it from other types of addresses.
data SegWitAddress = SegWitPubkey !Hash160 | SegWitScript !Hash256
  deriving (Eq, Show, Generic)

-- | Nothing if the given address is not SegWit.
guardSegWit :: Address -> Maybe SegWitAddress
guardSegWit a = case a of
  WitnessPubKeyAddress v -> Just $ SegWitPubkey v
  WitnessScriptAddress v -> Just $ SegWitScript v
  _                      -> Nothing

-- | Unwrap segwit to generic BTC address.
fromSegWit :: SegWitAddress -> Address
fromSegWit a = case a of
  SegWitPubkey v -> WitnessPubKeyAddress v
  SegWitScript v -> WitnessScriptAddress v

-- | Default value for P parameter (amount of bits in golomb rice encoding).
-- Set to fixed `19` according to BIP-158.
btcDefP :: Int
btcDefP = 19

-- | BIP 158 filter that tracks only Bech32 SegWit addresses that are used in specific block.
data BtcAddrFilter = BtcAddrFilter {
  btcAddrFilterN   :: !Word64 -- ^ the total amount of items in filter
, btcAddrFilterGcs :: !GCS -- ^ Actual encoded golomb encoded set
} deriving (Show, Generic)

-- | Encoding filter as simple <length><gcs>
encodeBtcAddrFilter :: BtcAddrFilter -> ByteString
encodeBtcAddrFilter BtcAddrFilter {..} =
  BSL.toStrict . B.toLazyByteString $ B.word64BE btcAddrFilterN <> B.byteString
    (encodeGcs btcAddrFilterGcs)

-- | Decoding filter from raw bytes
decodeBtcAddrFilter :: ByteString -> Either String BtcAddrFilter
decodeBtcAddrFilter = A.parseOnly (parser <* A.endOfInput)
 where
  parser =
    BtcAddrFilter
      <$> A.anyWord64be
      <*> fmap (decodeGcs btcDefP) A.takeByteString

-- | Contains each input tx for each tx in a block
type InputTxs = Map TxHash Tx

-- | Add each segwit transaction to filter. We add Bech32 addresses for outputs that
-- are used as inputs in the given block and addresses that are outputs of transactions
-- in the given block.
makeBtcFilter :: InputTxs -> Block -> BtcAddrFilter
makeBtcFilter txs block = undefined
 where
  outputSet =
    catMaybes $ concatMap (fmap getSegWitAddr . txOut) $ blockTxns block

-- | Extract segwit address from transaction output
getSegWitAddr :: TxOut -> Maybe SegWitAddress
getSegWitAddr tout = case decodeOutputBS $ scriptOutput tout of
  Right (PayWitnessPKHash     h) -> Just $ SegWitPubkey h
  Right (PayWitnessScriptHash h) -> Just $ SegWitScript h
  _                              -> Nothing

-- | Siphash key for filter is first 16 bytes of the hash (in standard little-endian representation)
-- of the block for which the filter is constructed. This ensures the key is deterministic while
-- still varying from block to block.
blockSipHash :: Block -> SipKey
blockSipHash =
  fromBs . BS.reverse . encode . getBlockHash . headerHash . blockHeader
 where
  toWord64 =
    fst . foldl (\(!acc, !i) b -> (acc + fromIntegral b ^ i, i + 1)) (0, 1)
  fromBs bs = SipKey (toWord64 $ BS.unpack . BS.take 8 $ bs)
                     (toWord64 $ BS.unpack . BS.take 8 . BS.drop 8 $ bs)

-- | Check that given address is located in the filter.
applyBtcFilter :: BtcAddrFilter -> SegWitAddress -> Bool
applyBtcFilter = undefined
