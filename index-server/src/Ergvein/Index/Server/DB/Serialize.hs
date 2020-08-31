module Ergvein.Index.Server.DB.Serialize
  (
    ErgSerialize(..)
  ) where

import Control.Monad (replicateM)
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Builder as BB
import Data.Foldable
import Data.Word

import Ergvein.Index.Server.DB.Schema.Filters
import Ergvein.Index.Server.DB.Schema.Indexer
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Types.Transaction

import qualified Data.Attoparsec.ByteString as Parse
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Data.Text.Encoding as TE

-- ===========================================================================
--           instance ErgSerialize ScannedHeightRec
-- ===========================================================================

instance ErgSerialize ScannedHeightRec where
  ergSerialize (ScannedHeightRec sh) = BL.toStrict . toLazyByteString $ word64LE sh
  ergDeserialize = parseOnly $ ScannedHeightRec <$> anyWord64le

-- ===========================================================================
--           instance ErgSerialize TxRec
-- ===========================================================================

instance ErgSerialize TxRec where
  ergSerialize    = serializeTxRec
  ergDeserialize  = deserializeTxRec

deserializeTxRec :: ByteString -> Either String TxRec
deserializeTxRec bs = flip parseOnly bs $ do
  txHashLen <- fromIntegral <$> anyWord16le
  txRecHash <- fmap (TxHash . BSS.toShort) $ Parse.take txHashLen
  txBytesLen <- fromIntegral <$> anyWord64le
  txRecBytes <- Parse.take txBytesLen
  txRecUnspentOutputsCount <- anyWord32le
  pure $ TxRec{..}

serializeTxRec :: TxRec -> ByteString
serializeTxRec TxRec{..} = BL.toStrict . toLazyByteString $
      word16LE txHashLen
  <> shortByteString txh
  <> word64LE txBytesLen
  <> byteString txRecBytes
  <> word32LE txRecUnspentOutputsCount
  where
    txh = getTxHash txRecHash
    txHashLen = fromIntegral $ BSS.length txh
    txBytesLen = fromIntegral $ BS.length txRecBytes

-- ===========================================================================
--           instance ErgSerialize BlockMetaRec
-- ===========================================================================

instance ErgSerialize BlockMetaRec where
  ergSerialize (BlockMetaRec hd filt) = BL.toStrict . toLazyByteString $ let
    len1 = fromIntegral $ BSS.length hd
    len2 = fromIntegral $ BS.length filt
    in word16LE len1 <> shortByteString hd <> word64LE len2 <> byteString filt
  ergDeserialize = parseOnly $ do
    len1 <- fromIntegral <$> anyWord16le
    blockMetaRecHeaderHashHexView <- fmap BSS.toShort $ Parse.take len1
    len2 <- fromIntegral <$> anyWord64le
    blockMetaRecAddressFilterHexView <- Parse.take len2
    pure BlockMetaRec{..}

-- ===========================================================================
--           instance ErgSerialize KnownPeerRecItem, KnownPeersRec
-- ===========================================================================

instance ErgSerialize KnownPeerRecItem where
  ergSerialize = BL.toStrict . toLazyByteString . knownPeerRecItemBuilder
  ergDeserialize = parseOnly knownPeerRecItemParser

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

instance ErgSerialize KnownPeersRec where
  ergSerialize (KnownPeersRec items) = BL.toStrict . toLazyByteString $ let
    els = knownPeerRecItemBuilder <$> items
    num = fromIntegral $ length els
    in word32LE num <> mconcat els

  ergDeserialize = parseOnly $ do
    num <- fmap fromIntegral anyWord32le
    fmap KnownPeersRec $ replicateM num knownPeerRecItemParser

-- ===========================================================================
--           instance ErgSerialize LastScannedBlockHeaderHashRec
-- ===========================================================================

instance ErgSerialize LastScannedBlockHeaderHashRec where
  ergSerialize (LastScannedBlockHeaderHashRec hs) = BL.toStrict . toLazyByteString $ let
    size = fromIntegral $ BSS.length hs
    in word16LE size <> shortByteString hs
  ergDeserialize = parseOnly $ do
    size <- fromIntegral <$> anyWord16le
    fmap (LastScannedBlockHeaderHashRec . BSS.toShort) $ Parse.take size

-- ===========================================================================
--           instance ErgSerialize ContentHistoryRecItem, ContentHistoryRec
-- ===========================================================================

instance ErgSerialize ContentHistoryRecItem where
  ergSerialize = BL.toStrict . toLazyByteString . contentHistoryRecItemBuilder
  ergDeserialize = parseOnly contentHistoryRecItemParser


instance ErgSerialize ContentHistoryRec where
  ergSerialize (ContentHistoryRec items) =
    let len = fromIntegral $ Seq.length items
        bs = fold $ fmap contentHistoryRecItemBuilder items
    in BL.toStrict . toLazyByteString $ word32LE len <> bs
  ergDeserialize = parseOnly $ do
    len <- fromIntegral <$> anyWord32le
    fmap (ContentHistoryRec . Seq.fromList) $ replicateM len contentHistoryRecItemParser

contentHistoryRecItemBuilder :: ContentHistoryRecItem -> Builder
contentHistoryRecItemBuilder (ContentHistoryRecItem spent addedHash) =
  let len = fromIntegral $ length addedHash
  in hash2Word32MapBuilder spent
      <> word32LE len
      <> mconcat (txHashBuilder <$> addedHash)

contentHistoryRecItemParser :: Parser ContentHistoryRecItem
contentHistoryRecItemParser = do
  spent <- hash2Word32MapParser
  num <- fmap fromIntegral anyWord32le
  addedHash <- replicateM num txHashParser
  pure $ ContentHistoryRecItem spent addedHash

txHashParser :: Parser TxHash
txHashParser = do
  len <- fromIntegral <$> anyWord16le
  fmap (TxHash . BSS.toShort) $ Parse.take len

txHashBuilder :: TxHash -> Builder
txHashBuilder (TxHash th) = word16LE (fromIntegral $ BSS.length th) <> shortByteString th

hash2Word32MapParser :: Parser (M.Map TxHash Word32)
hash2Word32MapParser = do
  num <- fromIntegral <$> anyWord32le
  fmap M.fromList $ replicateM num $ do
    th <- txHashParser
    c <- anyWord32le
    pure (th, c)

hash2Word32MapBuilder :: M.Map TxHash Word32 -> Builder
hash2Word32MapBuilder ms = word32LE num <> elb
  where
    els = M.toList ms
    num = fromIntegral $ M.size ms
    elb = mconcat $ flip fmap els $ \(th,c) -> txHashBuilder th <> word32LE c

-- ===========================================================================
--           instance ErgSerialize SchemaVersionRec
-- ===========================================================================
--
-- instance ErgSerialize SchemaVersionRec where
--   ergSerialize Text = "Text"
--   ergDeserialize = const $ Right Text
