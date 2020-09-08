-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc
  ( -- * Filter
    BtcAddrFilter(..)
  , encodeBtcAddrFilter
  , decodeBtcAddrFilter
  , btcAddrFilterHash
  , makeBtcFilter
  , applyBtcFilter
  -- * Reexports
  , module Ergvein.Filters.Btc.Index
  )
where

import           Crypto.Hash                    ( SHA256 (..), hashWith)
import           Data.ByteArray.Hash            ( SipKey(..) )
import           Data.ByteString                ( ByteString )
import           Data.Serialize                 ( encode )
import           Data.Word
import           Ergvein.Filters.Btc.Index
import           Ergvein.Filters.Btc.VarInt
import           Ergvein.Filters.GCS
import           GHC.Generics
import           Network.Haskoin.Block
import           Network.Haskoin.Transaction

import qualified Data.Attoparsec.ByteString    as A
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Vector                   as V

-- | BIP 158 filter that tracks only Bech32 SegWit addresses that are used in specific block.
data BtcAddrFilter = BtcAddrFilter {
  btcAddrFilterN   :: !Word64 -- ^ the total amount of items in filter
, btcAddrFilterGcs :: !GCS -- ^ Actual encoded golomb encoded set
} deriving (Show, Generic)

-- | Encoding filter as simple <length><gcs>
encodeBtcAddrFilter :: BtcAddrFilter -> ByteString
encodeBtcAddrFilter BtcAddrFilter {..} =
  BSL.toStrict . B.toLazyByteString $ encodeVarInt btcAddrFilterN <> B.byteString
    (encodeGcs btcAddrFilterGcs)

-- | Decoding filter from raw bytes
decodeBtcAddrFilter :: ByteString -> Either String BtcAddrFilter
decodeBtcAddrFilter = A.parseOnly (parser <* A.endOfInput)
 where
  parser =
    BtcAddrFilter
      <$> parseVarInt
      <*> fmap (decodeGcs btcDefP) A.takeByteString

-- | Calculate filter hash of filter based on previous filter hash
btcAddrFilterHash :: BtcAddrFilter -> FilterHash -> FilterHash
btcAddrFilterHash bf prev = FilterHash . sha256d $ sha256d (encodeBtcAddrFilter bf) <> unFilterHash prev
  where
    sha256d :: ByteString -> ByteString
    sha256d = BA.convert . hashWith SHA256 . hashWith SHA256

-- | Add scripts of tx outputs to filter.
--
-- To add outputs that is ergvein indexable, use `isErgveinIndexable`.
-- To add outputs that is compatible with BIP158, use `isBip158Indexable`.
makeBtcFilter :: forall m . HasTxIndex m => (ByteString -> Bool) -> Block -> m BtcAddrFilter
makeBtcFilter check block = do
  inputSet <- foldInputs collect [] block
  let totalSet = V.uniq $ V.fromList $ outputSet <> inputSet
      n = fromIntegral $ V.length totalSet
  pure BtcAddrFilter
      { btcAddrFilterN   = n
      , btcAddrFilterGcs = constructGcs btcDefP sipkey btcDefM totalSet
      }
 where
  collect :: [ByteString] -> ByteString -> m [ByteString]
  collect !as bs = pure $ if check bs then bs : as else as
  makeGcsSet = concatMap (fmap scriptOutput . txOut)
  outputSet = makeGcsSet $ blockTxns block
  sipkey    = blockSipHash . headerHash . blockHeader $ block

-- | Check that given address is located in the filter.
applyBtcFilter :: BlockHash -> BtcAddrFilter -> ByteString -> Bool
applyBtcFilter bhash BtcAddrFilter {..} item = matchGcs sipkey btcDefM btcAddrFilterN btcAddrFilterGcs item
 where
  sipkey = blockSipHash bhash
