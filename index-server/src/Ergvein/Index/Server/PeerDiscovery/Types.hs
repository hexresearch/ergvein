module Ergvein.Index.Server.PeerDiscovery.Types 
  ( Peer (..)
  , PeerCandidate (..)
  , PeerDiscoveryRequisites (..)
  )where

import Data.Set (Set)
import Data.Time
import Network.Socket
import Ergvein.Index.Protocol.Types

import qualified Data.Vector.Unboxed as UV

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