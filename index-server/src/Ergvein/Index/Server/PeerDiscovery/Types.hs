{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.PeerDiscovery.Types 
  ( Peer (..)
  , PeerCandidate (..)
  , PeerDiscoveryRequisites (..)
  , PeerIP (..)
  , PeerAddr (..)
  )where

import Data.Set (Set)
import Data.Word
import Data.Time
import Network.Socket
import Ergvein.Index.Protocol.Types
import Data.Flat

import qualified Data.Vector.Unboxed as UV

data PeerIP = V4 Word32
            | V6 (Word32, Word32, Word32, Word32)
  deriving (Generic, Show, Eq, Ord, Flat)

data PeerAddr = PeerAddr
  { peerAddrIP   :: PeerIP
  , peerAddrPort :: Word16
  } deriving (Generic, Show, Eq, Ord, Flat)

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