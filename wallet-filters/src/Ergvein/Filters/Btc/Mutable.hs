-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc.Mutable
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
  , applyBtcFilterMany
  -- * Testing
  , getSegWitAddr
  )
where

import           Control.Monad.IO.Class
import           Control.DeepSeq
import           Data.ByteArray.Hash            ( SipKey(..) )
import           Data.ByteString                ( ByteString )
import           Data.Map.Strict                ( Map )
import           Data.Maybe
import           Data.Serialize                 ( encode )
import           Data.Word
import           Ergvein.Filters.Btc.Address
import           Ergvein.Filters.GCS.Mutable
import           Ergvein.Types.Address          (btcAddrToString')
import           GHC.Generics
import           Network.Haskoin.Address
import           Network.Haskoin.Block
import           Network.Haskoin.Constants
import           Network.Haskoin.Crypto         ( Hash160
                                                , Hash256
                                                )
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction

import qualified Data.Attoparsec.Binary        as A
import qualified Data.Attoparsec.ByteString    as A
import qualified Data.Binary                   as B
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Map.Strict               as M
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V

-- | Default value for P parameter (amount of bits in golomb rice encoding).
-- Set to fixed `19` according to BIP-158.
btcDefP :: Int
btcDefP = 19

-- | Default value for M parameter (the target false positive rate).
-- Set to fixed `784931` according to BIP-158.
btcDefM :: Word64
btcDefM = 784931

-- | BIP 158 filter that tracks only Bech32 SegWit addresses that are used in specific block.
data BtcAddrFilter = BtcAddrFilter {
  btcAddrFilterN   :: !Word64 -- ^ the total amount of items in filter
, btcAddrFilterGcs :: !GCS -- ^ Actual encoded golomb encoded set
} deriving (Generic)

instance NFData BtcAddrFilter

-- | Encoding filter as simple <length><gcs>
encodeBtcAddrFilter :: MonadIO m => BtcAddrFilter -> m ByteString
encodeBtcAddrFilter BtcAddrFilter {..} = do
  bs <- encodeGcs btcAddrFilterGcs
  pure $ BSL.toStrict . B.toLazyByteString $
    B.word64BE btcAddrFilterN <> B.byteString bs

-- | Decoding filter from raw bytes
decodeBtcAddrFilter :: MonadIO m =>  ByteString -> m (Either String BtcAddrFilter)
decodeBtcAddrFilter bs = case A.parseOnly (parser <* A.endOfInput) bs of
  Left er -> pure $ Left er
  Right (w, gbs) -> do
   gcs <- decodeGcs btcDefP gbs
   pure . Right $ BtcAddrFilter w gcs
 where
  parser = (,) <$> A.anyWord64be <*> A.takeByteString

-- instance B.Binary BtcAddrFilter where
--   put = B.put . encodeBtcAddrFilter
--   {-# INLINE put #-}
--   get = do
--     bs <- B.get
--     either fail pure $ decodeBtcAddrFilter bs
--   {-# INLINE get #-}

-- | Contains each input tx for each tx in a block
type InputTxs = [Tx]

-- | Add each segwit transaction to filter. We add Bech32 addresses for outputs that
-- are used as inputs in the given block and addresses that are outputs of transactions
-- in the given block.
--
-- Network argument controls whether we are in testnet or mainnet.
makeBtcFilter :: MonadIO m => Network -> InputTxs -> Block -> m BtcAddrFilter
makeBtcFilter net intxs block = do
  gcs <- constructGcs btcDefP sipkey btcDefM totalSet
  pure BtcAddrFilter
      { btcAddrFilterN   = n
      , btcAddrFilterGcs = gcs
      }
 where
  makeSegWitSet = fmap (encodeSegWitAddress net) . catMaybes . concatMap
    (fmap getSegWitAddr . txOut)
  outputSet = makeSegWitSet $ blockTxns block
  inputSet  = makeSegWitSet intxs
  totalSet  = V.fromList $ outputSet <> inputSet
  n         = fromIntegral $ V.length totalSet
  sipkey    = blockSipHash . headerHash . blockHeader $ block

-- | Siphash key for filter is first 16 bytes of the hash (in standard little-endian representation)
-- of the block for which the filter is constructed. This ensures the key is deterministic while
-- still varying from block to block.
blockSipHash :: BlockHash -> SipKey
blockSipHash = fromBs . BS.reverse . encode . getBlockHash
 where
  toWord64 =
    fst . foldl (\(!acc, !i) b -> (acc + fromIntegral b ^ i, i + 1)) (0, 1)
  fromBs bs = SipKey (toWord64 $ BS.unpack . BS.take 8 $ bs)
                     (toWord64 $ BS.unpack . BS.take 8 . BS.drop 8 $ bs)

-- | Check that given address is located in the filter. Note that filter is destroyed after the opeeration.
applyBtcFilter :: MonadIO m => Network -> BlockHash -> BtcAddrFilter -> SegWitAddress -> m Bool
applyBtcFilter net bhash BtcAddrFilter {..} addr = matchGcs btcDefP
                                                            sipkey
                                                            btcDefM
                                                            btcAddrFilterN
                                                            btcAddrFilterGcs
                                                            item
 where
  item   = encodeSegWitAddress net addr
  sipkey = blockSipHash bhash

-- | Check that given address is located in the filter. Note that filter is destroyed after the operation.
applyBtcFilterMany :: MonadIO m => Network -> BlockHash -> BtcAddrFilter -> [SegWitAddress] -> m Bool
applyBtcFilterMany net bhash BtcAddrFilter {..} addrs = matchGcsMany btcDefP
                                                            sipkey
                                                            btcDefM
                                                            btcAddrFilterN
                                                            btcAddrFilterGcs
                                                            items
 where
  items  = fmap (encodeSegWitAddress net) addrs
  sipkey = blockSipHash bhash
