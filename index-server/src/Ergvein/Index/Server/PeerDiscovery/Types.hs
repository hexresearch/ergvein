{-# LANGUAGE DeriveAnyClass #-}
module Ergvein.Index.Server.PeerDiscovery.Types 
  ( Peer (..)
  , PeerCandidate (..)
  , PeerDiscoveryRequisites (..)
  , PeerIP (..)
  , PeerAddr (..)
  )where

import Data.Set (Set)
import Data.Text
import Data.Time
import Data.Time.Clock
import Data.Word
import Ergvein.Index.Protocol.Types
import Ergvein.Types.Currency
import Ergvein.Types.Transaction
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
