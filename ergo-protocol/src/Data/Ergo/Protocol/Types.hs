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
{-# LANGUAGE DuplicateRecordFields #-}
module Data.Ergo.Protocol.Types(
    Network(..)
  , ErgoMessage(..)
  , Message(..)
  , ProtoVer(..)
  , IP(..)
  , NetAddr(..)
  , StateType(..)
  , OperationModeFeature(..)
  , featureOperationModeId
  , SessionFeature(..)
  , sessionFeatureId
  , LocalAddressFeature(..)
  , localAddressFeatureId
  , PeerFeature(..)
  , featureId
  , Handshake(..)
  , handshakeId
  , handshakeTimeout
  , magicBytes
  , SyncInfo(..)
  , syncInfoId
  , InvMsg(..)
  , invMsgId
  , RequestModifierMsg(..)
  , requestModifierId
  , ModifierMsg(..)
  , modifierMsgId
  ) where

import Data.ByteString (ByteString)
import Data.Ergo.Modifier
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

-- | Ergo protocol is split into two parts: handshaking and operation.
--
-- Handshaking message is first message received and sent, no other message
-- is allowed until handshaking done.
data ErgoMessage = MsgHandshake !Handshake | MsgOther !Message
  deriving (Generic, Show, Read, Eq)

-- | Possible types of network messages in P2P protocol for Ergo
--
-- Handshake message is special and encoded separately. See `Handshake`
data Message =
    MsgSyncInfo !SyncInfo
  | MsgInv !InvMsg
  | MsgRequestModifier !RequestModifierMsg
  | MsgModifier !ModifierMsg
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

-- | Feature that contains local IPv4 address of node
data LocalAddressFeature = LocalAddressFeature
  { address :: !Word32
  , port    :: !Word32
  } deriving (Generic, Show, Read, Eq)

-- | ID of 'LocalAddressFeature' feature
localAddressFeatureId :: Word8
localAddressFeatureId = 2

-- | Known peer feature types that client supports.
data PeerFeature =
    FeatureOperationMode !OperationModeFeature
  | FeatureSession !SessionFeature
  | FeatureLocalAddress !LocalAddressFeature
  | UnknownFeature !Word8 !ByteString
  deriving (Generic, Show, Read, Eq)

-- | Get protocol type for feature type
featureId :: PeerFeature -> Word8
featureId v = case v of
  FeatureOperationMode _ -> featureOperationModeId
  FeatureSession _ -> sessionFeatureId
  FeatureLocalAddress _ -> localAddressFeatureId
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
    syncHeaders :: !(Vector BlockId)
  } deriving (Generic, Show, Read, Eq)

-- | ID of SyncInfo message type
syncInfoId :: Integral a => a
syncInfoId = 65

-- |  The `Inv` message (inventory message) transmits one or more inventories of
-- objects known to the transmitting peer.
-- It can be sent unsolicited to announce new transactions or blocks,
-- or it can be sent in reply to a `SyncInfo` message (or application-specific messages like `GetMempool`).
data InvMsg = InvMsg {
    typeId :: !ModifierType
  , ids    :: !(Vector ModifierId)
  } deriving (Generic, Show, Read, Eq)

-- | ID of inv message
invMsgId :: Integral a => a
invMsgId = 55

-- | The `RequestModifier` message requests one or more modifiers from another node.
-- The objects are requested by an inventory, which the requesting node
-- typically received previously by way of an `Inv` message.
--
-- This message cannot be used to request arbitrary data, such as historic transactions no
-- longer in the memory pool. Full nodes may not even be able to provide older blocks if
-- they’ve pruned old transactions from their block database.
-- For this reason, the `RequestModifier` message should usually only be used to request
-- data from a node which previously advertised it had that data by sending an `Inv` message.
data RequestModifierMsg = RequestModifierMsg {
    typeId :: !ModifierType
  , ids    :: !(Vector ModifierId)
  } deriving (Generic, Show, Read, Eq)

-- | ID of request modifier message
requestModifierId :: Integral a => a
requestModifierId = 22

-- | The `Modifier` message is a reply to a `RequestModifier` message which requested these modifiers.
data ModifierMsg = ModifierMsg {
    typeId    :: !ModifierType
  , modifiers :: !(Vector Modifier)
  } deriving (Generic, Show, Read, Eq)

-- | ID of modifier message
modifierMsgId :: Integral a => a
modifierMsgId = 33
