module Ergvein.Index.Server.DB.Serialize
  (
    EgvSerialize(..)
  , putTxInfosAsRecs
  , serializeWord32
  , deserializeWord32
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
import Ergvein.Index.Server.PeerDiscovery.Types
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
--           Tx-related
-- ===========================================================================

instance EgvSerialize TxRecMeta where
  egvSerialize _ = BL.toStrict . toLazyByteString . word32LE . unTxRecMeta
  egvDeserialize _ = fmap TxRecMeta . parseOnly anyWord32le

instance EgvSerialize TxRecBytes where
  egvSerialize _ = BL.toStrict . toLazyByteString . buildBS . unTxRecBytes
  egvDeserialize _ = fmap TxRecBytes . parseOnly parseBS

-- ===========================================================================
--           instance EgvSerialize BlockMetaRec
-- ===========================================================================

instance EgvSerialize BlockMetaRec where
  egvSerialize cur (BlockMetaRec hd filt) = BL.toStrict . toLazyByteString $ let
    len = fromIntegral $ BS.length filt
    in shortByteString hd <> word64LE len <> byteString filt
  egvDeserialize cur = parseOnly $ do
    blockMetaRecHeaderHash <- fmap BSS.toShort $ Parse.take (getBlockHashLength cur)
    len <- fromIntegral <$> anyWord64le
    blockMetaRecAddressFilter <- Parse.take len
    pure BlockMetaRec{..}

-- ===========================================================================
--           instance EgvSerialize KnownPeerRecItem, KnownPeersRec
-- ===========================================================================

instance EgvSerialize KnownPeerRecItem where
  egvSerialize _ = BL.toStrict . toLazyByteString . knownPeerRecItemBuilder
  egvDeserialize _ = parseOnly knownPeerRecItemParser

peerAddrBuilder :: PeerAddr -> Builder
peerAddrBuilder PeerAddr{..} = let
  ip = case peerAddrIP of
        V4 a  -> BB.word8 0 <> word32LE a
        V6 (a,b,c,d) -> BB.word8 1 <> word32LE a <> word32LE b <> word32LE c <> word32LE d
  in word16LE peerAddrPort <> ip

peerAddrParser :: Parser PeerAddr
peerAddrParser = do
  peerAddrPort <- anyWord16le
  isIpV6 <- (== 1) <$> anyWord8
  peerAddrIP <-
    if isIpV6 then
      V6 <$> ((,,,) <$> anyWord32le <*> anyWord32le <*> anyWord32le <*> anyWord32le)
    else
      V4 <$> anyWord32le
  pure PeerAddr {..}

knownPeerRecItemBuilder :: KnownPeerRecItem -> Builder
knownPeerRecItemBuilder KnownPeerRecItem{..} =
       peerAddrBuilder knownPeerRecAddr
    <> word32LE valLen
    <> byteString val
  where
    val = TE.encodeUtf8 knownPeerRecLastValidatedAt
    valLen = fromIntegral $ BS.length val

knownPeerRecItemParser :: Parser KnownPeerRecItem
knownPeerRecItemParser = do
  knownPeerRecAddr <- peerAddrParser
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
--           Rollback
-- ===========================================================================

instance EgvSerialize RollbackSequence where
  egvSerialize _ (RollbackSequence items) = BL.toStrict . toLazyByteString $
       word32LE (fromIntegral $ Seq.length items)
    <> fold (rollbackItemBuilder <$> items)
  egvDeserialize cur = parseOnly $ do
    len <- fromIntegral <$> anyWord32le
    fmap (RollbackSequence . Seq.fromList) $ replicateM len (rollbackItemParser cur)

rollbackItemBuilder :: RollbackRecItem -> Builder
rollbackItemBuilder (RollbackRecItem sp m prevHash prevH) =
     word32LE (fromIntegral $ length sp)
  <> mconcat (txHashBuilder <$> sp)
  <> hash2Word32MapBuilder m
  <> buildBSS prevHash
  <> word64LE prevH

rollbackItemParser :: Currency -> Parser RollbackRecItem
rollbackItemParser cur = do
  l <- fromIntegral <$> anyWord32le
  sp <- replicateM l $ txHashParser cur
  m <- hash2Word32MapParser cur
  prevHash <- parseBSS
  prevH <- anyWord64le
  pure $ RollbackRecItem sp m prevHash prevH

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
--           Some utils
-- ===========================================================================

instance EgvSerialize ByteString where
  egvSerialize _ bs = BL.toStrict . toLazyByteString $
    word16LE (fromIntegral $ BS.length bs) <> byteString bs
  egvDeserialize _ = parseOnly $ Parse.take . fromIntegral =<< anyWord16le

instance EgvSerialize Word32 where
  egvSerialize _ = BL.toStrict . toLazyByteString . word32LE
  egvDeserialize _ = parseOnly anyWord32le

serializeWord32 :: Word32 -> ByteString
serializeWord32 = BL.toStrict . toLazyByteString . word32LE
{-# INLINE serializeWord32 #-}

deserializeWord32 :: ByteString -> Either String Word32
deserializeWord32 = parseOnly anyWord32le
{-# INLINE deserializeWord32 #-}

putTxInfosAsRecs :: Currency -> [TxInfo] -> LDB.WriteBatch
putTxInfosAsRecs cur items = mconcat $ parMap rpar putI (force items)
  where
    putI TxInfo{..} = let
      p1 = LDB.Put (txRawKey txHash) $ egvSerialize cur $ TxRecBytes txBytes
      p2 = LDB.Put (txMetaKey txHash) $ egvSerialize cur $ TxRecMeta txOutputsCount
      in [p1, p2]
