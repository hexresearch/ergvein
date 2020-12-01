{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.PeerDiscovery.Types 
  ( Peer (..)
  , PeerCandidate (..)
  , PeerDiscoveryRequisites (..)
  , PeerIP (..)
  , PeerAddr (..)
  )where

import Conversion
import Data.Set (Set)
import Data.Time
import Data.Word
import Ergvein.Index.Protocol.Types
import Network.Socket
import GHC.Generics

import qualified Data.Vector.Unboxed as UV

data PeerIP = V4 Word32
            | V6 (Word32, Word32, Word32, Word32)
  deriving (Generic, Show, Eq, Ord)

data PeerAddr = PeerAddr
  { peerAddrIP   :: PeerIP
  , peerAddrPort :: Word16
  } deriving (Generic, Show, Eq, Ord)

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



instance Conversion SockAddr PeerAddr where
  convert = \case
    SockAddrInet  port   ip   -> PeerAddr (V4 ip) (fromInteger $ toInteger port)
    SockAddrInet6 port _ ip _ -> PeerAddr (V6 ip) (fromInteger $ toInteger port)
    SockAddrUnix{} -> error "SockAddrUnix is not supported"
    -- Use of SockAddrCan generates deprecation warnign
    _              -> error "SockAddrCan is not supported"

instance Conversion PeerAddr SockAddr where
  convert PeerAddr {..} = let
    port = fromInteger $ toInteger peerAddrPort
    in case peerAddrIP of
      V4 ip -> SockAddrInet port ip
      V6 ip -> SockAddrInet6 port 0 ip 0
