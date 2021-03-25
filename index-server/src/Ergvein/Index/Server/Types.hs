{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.Types
  (
    BlockMeta(..)
  , BlockInfo (..)
  , TxInfo
  , ScanProgressInfo(..)
  , CacheEntry(..)
  -- * Peer discovery types
  , Peer (..)
  , PeerCandidate (..)
  , PeerDiscoveryRequisites (..)
  , sockAddressToEgvAddress
  ) where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Set (Set)
import Data.Time
import Data.Word
import Data.Persist
import GHC.Generics
import Network.Haskoin.Transaction (OutPoint)
import Network.Socket
import qualified Data.Vector.Unboxed as UV

import Ergvein.Index.Protocol.Types
import Ergvein.Types

data BlockMeta = BlockMeta
  { blockMetaCurrency      :: !Currency
  , blockMetaBlockHeight   :: !BlockHeight
  , blockMetaHeaderHash    :: !ShortByteString
  , blockMetaPreviousHeaderBlockHash :: !ShortByteString
  , blockMetaAddressFilter :: !ByteString
  } deriving (Show, Generic)

instance NFData BlockMeta

data BlockInfo = BlockInfo
  { blockInfoMeta    :: !BlockMeta
  , spentTxOutputs   :: ![OutPoint]
  , createdTxOutputs :: ![TxInfo]
  } deriving (Show, Generic)
instance NFData BlockInfo

type TxInfo = (ByteString, [(Word32, ByteString)])

type CreatedIds = (ByteString, Word32)

data ScanProgressInfo = ScanProgressInfo
  { nfoCurrency      :: !Currency
  , nfoScannedHeight :: !BlockHeight
  , nfoActualHeight  :: !BlockHeight
  }

data CacheEntry = CacheEntry {
  cacheHeight   :: !BlockHeight
, cacheHash     :: !ShortByteString
, cacheSpent    :: ![OutPoint]
, cacheCreated  :: ![CreatedIds]
} deriving (Show, Generic)
instance NFData CacheEntry

-- Peer discovery types

data Peer = Peer
  { peerAddress          :: !SockAddr
  , peerLastValidatedAt  :: !UTCTime
  } deriving Show

data PeerCandidate = PeerCandidate
  { peerCandidateAddress    :: !SockAddr
  , peerCandidateScanBlocks :: !(UV.Vector ScanBlock)
  }

data PeerDiscoveryRequisites = PeerDiscoveryRequisites
  { descReqOwnAddress           :: !(Maybe SockAddr)
  , descReqPredefinedPeers      :: !(Set SockAddr)
  , descReqActualizationDelay   :: !Int
  , descReqActualizationTimeout :: !NominalDiffTime
  }


putUtcTime :: UTCTime -> Put ()
putUtcTime (UTCTime (ModifiedJulianDay d) dt)= put d >> put (diffTimeToPicoseconds dt)

getUtcTime :: Get UTCTime
getUtcTime = do
  d <- get
  dt <- get
  pure $ (UTCTime (ModifiedJulianDay d) (picosecondsToDiffTime dt))

putSockAddr :: SockAddr -> Put ()
putSockAddr sa = let p8 = put @Word8 in case sa of
  SockAddrInet port host ->
    p8 0 >> put @Word64 (fromIntegral port) >> put host
  SockAddrInet6 port fi (h1,h2,h3,h4) sid -> do
    p8 1 >> put @Word64 (fromIntegral port) >> mapM_ put [fi,h1,h2,h3,h4,sid]
  _ -> p8 2

getSockAddr :: Get SockAddr
getSockAddr = do
  c <- get @Word8
  case c of
    0 -> do
      p <- get @Word64
      h <- get
      pure $ SockAddrInet (fromIntegral p) h
    1 -> do
      p <- get @Word64
      [fi,h1,h2,h3,h4,sid] <- replicateM 6 get
      pure $ SockAddrInet6 (fromIntegral p) fi (h1,h2,h3,h4) sid
    _ -> fail "Address type is not supported"

instance Persist Peer where
  put (Peer sa t) = putSockAddr sa >> putUtcTime t
  get = Peer <$> getSockAddr <*> getUtcTime

sockAddressToEgvAddress :: SockAddr -> Maybe Address
sockAddressToEgvAddress sa = case sa of
  SockAddrInet port host -> Just $ AddressIpv4 host (fromIntegral port)
  SockAddrInet6 port _ (h1,h2,h3,h4) _ -> Just $ AddressIpv6 (IpV6 h1 h2 h3 h4) (fromIntegral port)
  _ -> Nothing
