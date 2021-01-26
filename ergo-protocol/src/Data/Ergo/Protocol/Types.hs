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
  , SessionFeature(..)
  , sessionFeatureId
  , PeerFeature(..)
  , featureId
  , Handshake(..)
  , handshakeId
  , handshakeTimeout
  , magicBytes
  , SyncInfo(..)
  , HeaderId(..)
  , encodeHeaderId
  , decodeHeaderId
  , nullHeader
  , syncInfoId
  ) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)
import Data.Word
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16

-- | Blockchain network tag. Testing or production.
data Network = Mainnet | Testnet
  deriving (Generic, Show, Read, Eq)

-- | Magic bytes in front of message
magicBytes :: Network -> Word32
magicBytes Mainnet = 0x01000204
magicBytes Testnet = 0x02000000

-- | Possible types of network messages in P2P protocol for Ergo
data Message =
    MsgHandshake !Handshake
  | MsgSyncInfo !SyncInfo
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

-- | Session peer feature introduced in 3.3.7
data SessionFeature = SessionFeature {
  networkMagic :: !Word32
, sessionId    :: !Word64 -- ^ 64 bits long random session id. For reference client, session id is currently used only to avoid connections to self
} deriving (Generic, Show, Read, Eq)

-- | ID of 'SessionFeature' feature
sessionFeatureId :: Word8
sessionFeatureId = 3

-- | Known peer feature types that client supports.
data PeerFeature =
    FeatureOperationMode !OperationModeFeature
  | FeatureSession !SessionFeature
  | UnknownFeature !Word8 !ByteString
  deriving (Generic, Show, Read, Eq)

-- | Get protocol type for feature type
featureId :: PeerFeature -> Word8
featureId v = case v of
  FeatureOperationMode _ -> featureOperationModeId
  FeatureSession _ -> sessionFeatureId
  UnknownFeature i _ -> i

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
handshakeId :: Integral a => a
handshakeId = 75

{-
  The `SyncInfo` message requests an `Inv` message that provides modifier ids
  required be sender to synchronize his blockchain with the recipient.
  It allows a peer which has been disconnected or started for the first
  time to get the data it needs to request the blocks it hasn't seen.

  Payload of this message should be determined in underlying applications.
-}
data SyncInfo = SyncInfo {
    syncHeaders :: !(Vector HeaderId)
  } deriving (Generic, Show, Read, Eq)

-- | 32 Byte header hash
newtype HeaderId = HeaderId { unHeaderId :: ByteString }
  deriving(Eq, Ord, Show, Read)

-- | Convert header to hex string
encodeHeaderId :: HeaderId -> Text
encodeHeaderId = decodeUtf8 . B16.encode . unHeaderId

-- | Convert hex string to header hash. Need to be 32 byte length.
decodeHeaderId :: Text -> Maybe HeaderId
decodeHeaderId = check . fst . B16.decode . encodeUtf8
  where
    check bs | BS.length bs == 32 = Just $ HeaderId bs
             | otherwise = Nothing

-- | Header id that is filled with zeros. It is used as request for recent
-- headers.
nullHeader :: HeaderId
nullHeader = HeaderId $ BS.replicate 32 0

-- | ID of SyncInfo message type
syncInfoId :: Integral a => a
syncInfoId = 65
