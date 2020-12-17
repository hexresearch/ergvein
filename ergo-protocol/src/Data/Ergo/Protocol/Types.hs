{-|
Module      : Data.Ergo.Protocol.Types
Description : Types declaration for messages for P2P protocol for Ergo platform.
Copyright   : (c) 2020 ATUM SOLUTIONS AG
License     : MIT
Maintainer  : ncrashed@protonmail.com
Stability   : experimental
Portability : POSIX

The module contains only data types and serialization and deserialization code
for network messages in peer to peer protocol that is used in Ergo cryptocurrency.

Types are kept close to binary representation for fast processing.
-}
module Data.Ergo.Protocol.Types(
    Network(..)
  , Message(..)
  , ProtoVer(..)
  , IP(..)
  , NetAddr(..)
  , StateType(..)
  , OperationModeFeature(..)
  , featureOperationModeId
  , PeerFeature(..)
  , Handshake(..)
  , handshakeId
  , handshakeTimeout
  , magicBytes
  ) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word
import GHC.Generics

-- | Blockchain network tag. Testing or production.
data Network = Mainnet | Testnet
  deriving (Generic, Show, Read, Eq)

-- | Magic bytes in front of message
magicBytes :: Network -> Word32
magicBytes Mainnet = 0x01000204
magicBytes Testnet = 0x02000000

-- | Possible types of network messages in P2P protocol for Ergo
data Message = MsgHandshake !Handshake
  deriving (Generic, Show, Read, Eq)

-- | Protocol version
data ProtoVer = ProtoVer !Word8 !Word8 !Word8
  deriving (Generic, Show, Read, Eq)

-- | IP address of network address
data IP
  = IPV4 !Word32
  | IPV6 !Word32 !Word32 !Word32 !Word32
  deriving (Generic, Show, Read, Eq)

-- | IP and port
data NetAddr = NetAddr !IP !Word32
  deriving (Generic, Show, Read, Eq)

-- | State representation of the node
data StateType = StateUtxo | StateDigest
  deriving (Generic, Show, Read, Eq)

-- | Operation mode information
data OperationModeFeature = OperationModeFeature {
  stateType     :: !StateType
, verifying     :: !Bool -- ^ Whether peer verify transactions
, nipopowSuffix :: !(Maybe Word32) -- ^ Suffix length for NiPoPoW bootstrapping, 'Nothing' no NiPoPoW bootstrapping
, blocksStored  :: !Int32 -- ^ How many block kept, -1 means all are stored
} deriving (Generic, Show, Read, Eq)

-- | ID of 'OperationModeFeature' feature
featureOperationModeId :: Word8
featureOperationModeId = 16

-- | Known peer feature types that client supports.
data PeerFeature = FeatureOperationMode !OperationModeFeature | UnknownFeature !Word8 !ByteString
  deriving (Generic, Show, Read, Eq)

-- | Amount of seconds that is given to peer to send handshake message. After that
-- connection is dropped.
handshakeTimeout :: Int
handshakeTimeout = 30

-- | Handshake message is sent first
data Handshake = Handshake
  { time         :: !Int64
  , agentName    :: !Text
  , version      :: !ProtoVer
  , peerName     :: !Text
  , publicAddr   :: !(Maybe NetAddr)
  , peerFeatures :: !(Vector PeerFeature)
  } deriving (Generic, Show, Read, Eq)

-- | ID of handshake message type
handshakeId :: Word8
handshakeId = 75
