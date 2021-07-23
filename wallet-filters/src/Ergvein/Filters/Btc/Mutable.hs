-- | Implements BIP-158 like filter for Bech32 addresses. Note
-- that IT IS NOT exact BIP-158 as we don't put all public and redeem scripts
-- inside the filter to save bandwidth.
{-# LANGUAGE BangPatterns #-}
module Ergvein.Filters.Btc.Mutable
  ( -- * Filter
    BtcAddrFilter(..)
  , encodeBtcAddrFilter
  , decodeBtcAddrFilter
  , btcAddrFilterHash
  , makeBtcFilter
  , matchBtcTx
  , applyBtcFilter
  , applyBtcFilterMany
  , applyBtcPrefixFilterMany
  -- * Reexports
  , module Ergvein.Filters.Btc.Index
  )
where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Crypto.Hash                    ( SHA256 (..), hashWith)
import           Data.ByteString                ( ByteString )
import           Data.Word
import           Ergvein.Filters.Btc.Index
import           Ergvein.Filters.Btc.VarInt
import           Ergvein.Filters.GCS.Mutable
import           GHC.Generics
import           Network.Haskoin.Block
import           Network.Haskoin.Transaction

import qualified Data.Attoparsec.ByteString    as A
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Builder       as B
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.HashSet                  as HS

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
    encodeVarInt btcAddrFilterN <> B.byteString bs

-- | Decoding filter from raw bytes
decodeBtcAddrFilter :: MonadIO m =>  ByteString -> m (Either String BtcAddrFilter)
decodeBtcAddrFilter bs = case A.parseOnly (parser <* A.endOfInput) bs of
  Left er -> pure $ Left er
  Right (w, gbs) -> do
   gcs <- decodeGcs btcDefP gbs
   pure . Right $ BtcAddrFilter w gcs
 where
  parser = (,) <$> parseVarInt <*> A.takeByteString

-- | Calculate filter hash of filter based on previous filter hash
btcAddrFilterHash :: MonadIO m => BtcAddrFilter -> FilterHash -> m FilterHash
btcAddrFilterHash bf prev = do
  cnt <- encodeBtcAddrFilter bf
  pure $ FilterHash . BS.reverse . sha256d $ sha256d cnt <> BS.reverse (unFilterHash prev)
  where
    sha256d :: ByteString -> ByteString
    sha256d = BA.convert . hashWith SHA256 . hashWith SHA256

-- | Add scripts of tx outputs to filter.
--
-- To add outputs that is ergvein indexable, use `isErgveinIndexable`.
-- To add outputs that is compatible with BIP158, use `isBip158Indexable`.
makeBtcFilter :: forall m . (MonadIO m, HasTxIndex m) => (ByteString -> Bool) -> Block -> m BtcAddrFilter
makeBtcFilter check block = do
  inputSet <- foldInputs collect [] block
  let totalSet = HS.fromList $ outputSet <> inputSet
      n = fromIntegral $ HS.size totalSet
  gcs <- constructGcs btcDefP sipkey btcDefM totalSet
  pure BtcAddrFilter
      { btcAddrFilterN   = n
      , btcAddrFilterGcs = gcs
      }
 where
  collect :: [ByteString] -> ByteString -> m [ByteString]
  collect !as bs = pure $ if check bs then bs : as else as
  makeGcsSet = concatMap (filter check . fmap scriptOutput . txOut)
  outputSet = makeGcsSet $ blockTxns block
  sipkey    = blockSipHash . headerHash . blockHeader $ block

-- | Check that given transaction is located in the filter. Note that filter is destroyed after the opeeration.
--
-- Nothing is returned if we cannot find corresponding tx input in the context.
matchBtcTx :: forall m . (MonadIO m, HasTxIndex m) => BlockHash -> BtcAddrFilter -> Tx -> m (Maybe Bool)
matchBtcTx bhash bfilter tx = do
  minputs <- queryInputs
  case minputs of
    Nothing -> pure Nothing
    Just inputs -> do
      let outputs = fmap scriptOutput . txOut $! tx
      fmap Just $ applyBtcFilterMany bhash bfilter $ inputs ++ outputs
  where
    queryInputs = do
      mouts <- traverse queryOutPoint $ fmap prevOutput . txIn $! tx
      let mouts' :: Maybe [ByteString] = sequence mouts
      pure mouts'

-- | Check that given address is located in the filter. Note that filter is destroyed after the opeeration.
applyBtcFilter :: MonadIO m => BlockHash -> BtcAddrFilter -> ByteString -> m Bool
applyBtcFilter bhash BtcAddrFilter {..} item = matchGcs sipkey
                                                        btcDefM
                                                        btcAddrFilterN
                                                        btcAddrFilterGcs
                                                        item
 where
  sipkey = blockSipHash bhash

-- | Check that given address is located in the filter. Note that filter is destroyed after the operation.
applyBtcFilterMany :: MonadIO m => BlockHash -> BtcAddrFilter -> [ByteString] -> m Bool
applyBtcFilterMany bhash BtcAddrFilter {..} items = matchGcsMany sipkey
                                                                 btcDefM
                                                                 btcAddrFilterN
                                                                 btcAddrFilterGcs
                                                                 items
 where
  sipkey = blockSipHash bhash

-- | Check that given address is located in the filter. Note that filter is destroyed after the operation.
applyBtcPrefixFilterMany :: MonadIO m => (Word8, Word8) -> BtcAddrFilter -> [ByteString] -> m Bool
applyBtcPrefixFilterMany pref BtcAddrFilter {..} items = matchGcsMany sipkey
                                                                 btcDefM
                                                                 btcAddrFilterN
                                                                 btcAddrFilterGcs
                                                                 items
 where
  sipkey = prefSipHash pref
