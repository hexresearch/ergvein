{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.DB.Schema.Indexer
  (
    KnownPeerRecItem(..)
  , KnownPeersRec(..)
  , LastScannedBlockHeaderHashRec(..)
  , RollbackKey(..)
  , RollbackRecItem(..)
  , RollbackSequence(..)
  , rollbackKey
  , knownPeersRecKey
  , lastScannedBlockHeaderHashRecKey
  , schemaVersionRecKey
  , schemaVersion
  ) where

import Control.Monad
import Crypto.Hash.SHA256
import Data.Attoparsec.Binary
import Data.Attoparsec.ByteString as Parse
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Builder
import Data.FileEmbed
import Data.Foldable
import GHC.Generics
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Word

import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Index.Server.PeerDiscovery.Types (PeerAddr(..), PeerIP(..))
import qualified Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Short   as BSS
import qualified Data.ByteString.Builder as BB
import qualified Data.Serialize          as S
import qualified Data.Sequence           as Seq
import qualified Data.Map.Strict         as Map
import qualified Data.Text.Encoding      as TE

data KeyPrefix = Peer | LastBlockHash | Rollback | SchemaVersion deriving Enum

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Indexer.hs")

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

--PeerDiscovery

knownPeersRecKey :: ByteString
knownPeersRecKey  = keyString Peer $ mempty @String

data KnownPeersRec = KnownPeersRec {
    unKnownPeersRec :: [KnownPeerRecItem]
  } deriving (Generic, Show, Eq, Ord)

data KnownPeerRecItem = KnownPeerRecItem
  { knownPeerRecAddr             :: DiscoveryTypes.PeerAddr
  , knownPeerRecLastValidatedAt  :: Text
  } deriving (Generic, Show, Eq, Ord)

--lastScannedBlockHeaderHash

lastScannedBlockHeaderHashRecKey :: Currency -> ByteString
lastScannedBlockHeaderHashRecKey  = keyString LastBlockHash . LastScannedBlockHeaderHashRecKey

data LastScannedBlockHeaderHashRecKey = LastScannedBlockHeaderHashRecKey
  { lastScannedBlockHeaderHashRecKeyCurrency :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data LastScannedBlockHeaderHashRec = LastScannedBlockHeaderHashRec
  { lastScannedBlockHeaderHashRecHash :: !ShortByteString
  } deriving (Generic, Show, Eq, Ord)

-- Rollback

rollbackKey :: Currency -> ByteString
rollbackKey = keyString Rollback . RollbackKey

data RollbackKey = RollbackKey { unRollbackKey :: !Currency }
  deriving (Generic, Show, Eq, Ord, Serialize)

data RollbackRecItem = RollbackRecItem
  { rollbackItemAdded     :: [TxHash]
  , rollbackItemSpendings :: Map.Map TxHash Word32
  , rollbackPrevBlockHash :: !BlockHash
  , rollbackPrevHeight    :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

data RollbackSequence = RollbackSequence { unRollbackSequence :: Seq.Seq RollbackRecItem}
  deriving (Generic, Show, Eq, Ord)

--SchemaVersion

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

data SchemaVersionRec = Text  deriving (Generic, Show, Eq, Ord)


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
txHashParser BTC = do
  val <- Parse.take $ getTxHashLength BTC
  case S.runGet S.get val of
    Left err -> fail err
    Right result -> pure $ BtcTxHash result
txHashParser ERGO = fmap (ErgTxHash . BSS.toShort) $ Parse.take $ getTxHashLength ERGO

txHashBuilder :: TxHash -> Builder
txHashBuilder (BtcTxHash th) = byteString $ S.runPut $ S.put th
txHashBuilder (ErgTxHash th) = shortByteString th

hash2Word32MapParser :: Currency -> Parser (Map.Map TxHash Word32)
hash2Word32MapParser cur = do
  num <- fromIntegral <$> anyWord32le
  fmap Map.fromList $ replicateM num $ do
    th <- txHashParser cur
    c <- anyWord32le
    pure (th, c)

hash2Word32MapBuilder :: Map.Map TxHash Word32 -> Builder
hash2Word32MapBuilder ms = word32LE num <> elb
  where
    els = Map.toList ms
    num = fromIntegral $ Map.size ms
    elb = mconcat $ flip fmap els $ \(th,c) -> txHashBuilder th <> word32LE c
