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

import Conversion
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
import Data.Text (Text,pack,unpack)

import Ergvein.Index.Protocol.Types (Address(..), IpV6(..))
import Ergvein.Index.Protocol.Utils
import Ergvein.Index.Server.DB.Serialize.Class
import Ergvein.Index.Server.PeerDiscovery.Types (PeerAddr(..), PeerIP(..))
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
import qualified Ergvein.Index.Server.PeerDiscovery.Types as DiscoveryTypes
 
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Short   as BSS
import qualified Data.ByteString.Builder as BB
import qualified Data.Serialize          as S
import qualified Data.Sequence           as Seq
import qualified Data.Text.Encoding      as TE

data KeyPrefix
  = SchemaVersion
  | Peer
  | LastBlockHash
  | Rollback
  deriving Enum

keyString :: (Serialize k) => KeyPrefix -> k -> ByteString
keyString keyPrefix key = (fromIntegral $ fromEnum keyPrefix) `BS.cons` S.encode key

-- ===========================================================================
--           Schema Version
-- ===========================================================================

schemaVersion :: ByteString
schemaVersion = hash $(embedFile "src/Ergvein/Index/Server/DB/Schema/Indexer.hs")

schemaVersionRecKey :: ByteString
schemaVersionRecKey  = keyString SchemaVersion $ mempty @String

-- ===========================================================================
--           Known peers
-- ===========================================================================

knownPeersRecKey :: ByteString
knownPeersRecKey  = keyString Peer $ mempty @String

data KnownPeersRec = KnownPeersRec {
    unKnownPeersRec :: [KnownPeerRecItem]
  } deriving (Generic, Show, Eq, Ord)

data KnownPeerRecItem = KnownPeerRecItem
  { knownPeerRecAddr             :: DiscoveryTypes.PeerAddr
  , knownPeerRecLastValidatedAt  :: Text
  } deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Last scanned block header
-- ===========================================================================

lastScannedBlockHeaderHashRecKey :: Currency -> ByteString
lastScannedBlockHeaderHashRecKey  = keyString LastBlockHash . LastScannedBlockHeaderHashRecKey

data LastScannedBlockHeaderHashRecKey = LastScannedBlockHeaderHashRecKey
  { lastScannedBlockHeaderHashRecKeyCurrency :: !Currency
  } deriving (Generic, Show, Eq, Ord, Serialize)

data LastScannedBlockHeaderHashRec = LastScannedBlockHeaderHashRec
  { lastScannedBlockHeaderHashRecHash :: !ShortByteString
  } deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Rollback info
-- ===========================================================================

rollbackKey :: Currency -> ByteString
rollbackKey = keyString Rollback . RollbackKey

data RollbackKey = RollbackKey { unRollbackKey :: !Currency }
  deriving (Generic, Show, Eq, Ord, Serialize)

data RollbackRecItem = RollbackRecItem
  { rollbackItemAdded     :: [TxHash]
  , rollbackPrevBlockHash :: !BlockHash
  , rollbackPrevHeight    :: !BlockHeight
  } deriving (Generic, Show, Eq, Ord)

data RollbackSequence = RollbackSequence { unRollbackSequence :: Seq.Seq RollbackRecItem}
  deriving (Generic, Show, Eq, Ord)

-- ===========================================================================
--           Conversion instances
-- ===========================================================================

instance Conversion DiscoveryTypes.Peer KnownPeerRecItem where
  convert DiscoveryTypes.Peer{..} = let
    validatedAt = pack $ show $ peerLastValidatedAt
    in KnownPeerRecItem
      { knownPeerRecAddr = convert peerAddress
      , knownPeerRecLastValidatedAt = validatedAt
      }

instance Conversion KnownPeerRecItem DiscoveryTypes.Peer where
  convert KnownPeerRecItem {..} = DiscoveryTypes.Peer
    { peerAddress = convert knownPeerRecAddr
    , peerLastValidatedAt = read $ unpack knownPeerRecLastValidatedAt
    }

instance Conversion KnownPeerRecItem Address where
  convert KnownPeerRecItem {..} = let
    in case peerAddrIP knownPeerRecAddr of
      V4 ip -> AddressIpv4
        { addressPort    = peerAddrPort knownPeerRecAddr
        , addressV4      = ip
        }
      V6 (a,b,c,d) -> AddressIpv6
        { addressPort    = peerAddrPort knownPeerRecAddr
        , addressV6      = IpV6 a b c d
        }

-- ===========================================================================
--           instance EgvSerialize KnownPeerRecItem, KnownPeersRec
-- ===========================================================================

instance EgvSerialize KnownPeerRecItem where
  egvSerialize _ = BL.toStrict . toLazyByteString . knownPeerRecItemBuilder
  egvDeserialize _ = parseTillEndOfInput knownPeerRecItemParser

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

  egvDeserialize _ = parseTillEndOfInput $ do
    num <- fmap fromIntegral anyWord32le
    fmap KnownPeersRec $ replicateM num knownPeerRecItemParser



instance EgvSerialize LastScannedBlockHeaderHashRec where
  egvSerialize _ (LastScannedBlockHeaderHashRec hs) = BL.toStrict . toLazyByteString $ shortByteString hs
  egvDeserialize cur = parseTillEndOfInput $ do
    fmap (LastScannedBlockHeaderHashRec . BSS.toShort) $ Parse.take (getTxHashLength cur)


-- ===========================================================================
--           Rollback
-- ===========================================================================

instance EgvSerialize RollbackSequence where
  egvSerialize _ (RollbackSequence items) = BL.toStrict . toLazyByteString $
       word32LE (fromIntegral $ Seq.length items)
    <> fold (rollbackItemBuilder <$> items)
  egvDeserialize cur = parseTillEndOfInput $ do
    len <- fromIntegral <$> anyWord32le
    fmap (RollbackSequence . Seq.fromList) $ replicateM len (rollbackItemParser cur)

rollbackItemBuilder :: RollbackRecItem -> Builder
rollbackItemBuilder (RollbackRecItem sp prevHash prevH) =
     word32LE (fromIntegral $ length sp)
  <> mconcat (txHashBuilder <$> sp)
  <> buildBSS prevHash
  <> word64LE prevH

rollbackItemParser :: Currency -> Parser RollbackRecItem
rollbackItemParser cur = do
  l <- fromIntegral <$> anyWord32le
  sp <- replicateM l $ txHashParser cur
  prevHash <- parseBSS
  prevH <- anyWord64le
  pure $ RollbackRecItem sp prevHash prevH

txHashParser :: Currency -> Parser TxHash
txHashParser BTC = do
  val <- Parse.take $ getTxHashLength BTC
  case S.runGet S.get val of
    Left err -> fail err
    Right result -> pure $ BtcTxHash result
txHashParser ERGO = fmap (ErgTxHash . ErgTxId . BSS.toShort) $ Parse.take $ getTxHashLength ERGO

txHashBuilder :: TxHash -> Builder
txHashBuilder (BtcTxHash th) = byteString $ S.runPut $ S.put th
txHashBuilder (ErgTxHash th) = shortByteString . unErgTxId $ th
