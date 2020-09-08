module Ergvein.Index.Server.DB.Serialize
  (
    EgvSerialize(..)
  , putTxInfosAsRecs
  , serializeToTxRec
  , module Ergvein.Index.Server.DB.Serialize.Tx
  ) where

import Control.DeepSeq
import Control.Monad (replicateM)
import Control.Parallel.Strategies
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Builder as BB
import Data.Foldable
import Data.Word

import Ergvein.Index.Server.DB.Serialize.Tx
import Ergvein.Index.Server.BlockchainScanning.Types
import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Currency
import Ergvein.Types.Transaction

import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as TE
import qualified Database.LevelDB as LDB

-- ===========================================================================
--           instance EgvSerialize ScannedHeightRec
-- ===========================================================================

instance EgvSerialize ScannedHeightRec where
  egvSerialize _ (ScannedHeightRec sh) = BL.toStrict . toLazyByteString $ word64LE sh
  egvDeserialize _ = parseOnly $ ScannedHeightRec <$> anyWord64le

-- ===========================================================================
--           instance EgvSerialize TxRec
-- ===========================================================================

instance EgvSerialize TxRec where
  egvSerialize    = serializeTxRec
  egvDeserialize  = deserializeTxRec

deserializeTxRec :: Currency -> ByteString -> Either String TxRec
deserializeTxRec cur bs = flip parseOnly bs $ do
  txRecHash <- fmap (TxHash . BSS.toShort) $ Parse.take (getTxHashLength cur)
  txBytesLen <- fromIntegral <$> anyWord64le
  txRecBytes <- Parse.take txBytesLen
  txRecUnspentOutputsCount <- anyWord32le
  pure $ TxRec{..}

serializeTxRec :: Currency -> TxRec -> ByteString
serializeTxRec cur TxRec{..} = BL.toStrict . toLazyByteString $
     shortByteString txh
  <> word64LE txBytesLen
  <> byteString txRecBytes
  <> word32LE txRecUnspentOutputsCount
  where
    txh = getTxHash txRecHash
    txHashLen = fromIntegral $ BSS.length txh
    txBytesLen = fromIntegral $ BS.length txRecBytes

-- ===========================================================================
--           instance EgvSerialize BlockMetaRec
-- ===========================================================================

instance EgvSerialize BlockMetaRec where
  egvSerialize cur (BlockMetaRec hd filt) = BL.toStrict . toLazyByteString $ let
    len = fromIntegral $ BS.length filt
    in shortByteString hd <> word64LE len <> byteString filt
  egvDeserialize cur = parseOnly $ do
    blockMetaRecHeaderHashHexView <- fmap BSS.toShort $ Parse.take (getBlockHashLength cur)
    len <- fromIntegral <$> anyWord64le
    blockMetaRecAddressFilterHexView <- Parse.take len
    pure BlockMetaRec{..}

-- ===========================================================================
--           instance EgvSerialize KnownPeerRecItem, KnownPeersRec
-- ===========================================================================

instance EgvSerialize KnownPeerRecItem where
  egvSerialize _ = BL.toStrict . toLazyByteString . knownPeerRecItemBuilder
  egvDeserialize _ = parseOnly knownPeerRecItemParser

knownPeerRecItemBuilder :: KnownPeerRecItem -> Builder
knownPeerRecItemBuilder KnownPeerRecItem{..} =
       word16LE urlLen
    <> byteString url
    <> BB.word8 boo
    <> word32LE valLen
    <> byteString val
  where
    url = TE.encodeUtf8 knownPeerRecUrl
    urlLen = fromIntegral $ BS.length url
    val = TE.encodeUtf8 knownPeerRecLastValidatedAt
    valLen = fromIntegral $ BS.length val
    boo = if knownPeerRecIsSecureConn then 1 else 0

knownPeerRecItemParser :: Parser KnownPeerRecItem
knownPeerRecItemParser = do
  urlLen <- fromIntegral <$> anyWord16le
  knownPeerRecUrl <- fmap TE.decodeUtf8 $ Parse.take urlLen
  knownPeerRecIsSecureConn <- fmap (== 1) $ anyWord8
  valLen <- fromIntegral <$> anyWord32le
  knownPeerRecLastValidatedAt <- fmap TE.decodeUtf8 $ Parse.take valLen
  pure KnownPeerRecItem{..}

instance EgvSerialize KnownPeersRec where
  egvSerialize _ (KnownPeersRec items) = BL.toStrict . toLazyByteString $ let
    els = knownPeerRecItemBuilder <$> items
    num = fromIntegral $ length els
    in word32LE num <> mconcat els

  egvDeserialize _ = parseOnly $ do
    num <- fmap fromIntegral anyWord32le
    fmap KnownPeersRec $ replicateM num knownPeerRecItemParser

-- ===========================================================================
--           instance EgvSerialize LastScannedBlockHeaderHashRec
-- ===========================================================================

instance EgvSerialize LastScannedBlockHeaderHashRec where
  egvSerialize _ (LastScannedBlockHeaderHashRec hs) = BL.toStrict . toLazyByteString $ shortByteString hs
  egvDeserialize cur = parseOnly $ do
    fmap (LastScannedBlockHeaderHashRec . BSS.toShort) $ Parse.take (getTxHashLength cur)

-- ===========================================================================
--           instance EgvSerialize ContentHistoryRecItem, ContentHistoryRec
-- ===========================================================================

instance EgvSerialize ContentHistoryRecItem where
  egvSerialize _ = BL.toStrict . toLazyByteString . contentHistoryRecItemBuilder
  egvDeserialize = parseOnly . contentHistoryRecItemParser


instance EgvSerialize ContentHistoryRec where
  egvSerialize _ (ContentHistoryRec items) =
    let len = fromIntegral $ Seq.length items
        bs = fold $ fmap contentHistoryRecItemBuilder items
    in BL.toStrict . toLazyByteString $ word32LE len <> bs
  egvDeserialize cur = parseOnly $ do
    len <- fromIntegral <$> anyWord32le
    fmap (ContentHistoryRec . Seq.fromList) $ replicateM len (contentHistoryRecItemParser cur)

contentHistoryRecItemBuilder :: ContentHistoryRecItem -> Builder
contentHistoryRecItemBuilder (ContentHistoryRecItem spent addedHash) =
  let len = fromIntegral $ length addedHash
  in hash2Word32MapBuilder spent
      <> word32LE len
      <> mconcat (txHashBuilder <$> addedHash)

contentHistoryRecItemParser :: Currency -> Parser ContentHistoryRecItem
contentHistoryRecItemParser cur = do
  spent <- hash2Word32MapParser cur
  num <- fmap fromIntegral anyWord32le
  addedHash <- replicateM num (txHashParser cur)
  pure $ ContentHistoryRecItem spent addedHash

txHashParser :: Currency -> Parser TxHash
txHashParser cur = fmap (TxHash . BSS.toShort) $ Parse.take $ getTxHashLength cur

txHashBuilder :: TxHash -> Builder
txHashBuilder (TxHash th) = shortByteString th

hash2Word32MapParser :: Currency -> Parser (M.Map TxHash Word32)
hash2Word32MapParser cur = do
  num <- fromIntegral <$> anyWord32le
  fmap M.fromList $ replicateM num $ do
    th <- txHashParser cur
    c <- anyWord32le
    pure (th, c)

hash2Word32MapBuilder :: M.Map TxHash Word32 -> Builder
hash2Word32MapBuilder ms = word32LE num <> elb
  where
    els = M.toList ms
    num = fromIntegral $ M.size ms
    elb = mconcat $ flip fmap els $ \(th,c) -> txHashBuilder th <> word32LE c

-- ===========================================================================
--           instance EgvSerialize SchemaVersionRec
-- ===========================================================================
--

instance EgvSerialize ByteString where
  egvSerialize _ bs = BL.toStrict . toLazyByteString $
    word16LE (fromIntegral $ BS.length bs) <> byteString bs
  egvDeserialize _ = parseOnly $ Parse.take . fromIntegral =<< anyWord16le

-- ===========================================================================
--           Some utils
-- ===========================================================================

putTxInfosAsRecs :: Currency -> [TxInfo] -> LDB.WriteBatch
putTxInfosAsRecs cur items = fmap (uncurry LDB.Put) $ parMap rdeepseq putI (force items)
  where putI item = (txRecKey $ txHash $ item, ) $ serializeToTxRec cur item

serializeToTxRec :: Currency -> TxInfo -> BS.ByteString
serializeToTxRec cur TxInfo{..} = BL.toStrict . toLazyByteString $
      byteString txh
  <> word64LE txBytesLen
  <> byteString txBytes
  <> word32LE txOutputsCount
  where
    txh = BSS.fromShort $ getTxHash txHash
    txHashLen = fromIntegral $ BS.length txh
    txBytesLen = fromIntegral $ BS.length txBytes
