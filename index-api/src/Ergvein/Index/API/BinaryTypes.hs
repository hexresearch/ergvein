module Ergvein.Index.API.BinaryTypes where

import Data.Word

data MessageType = Version
                 | VersionACK
                 | FiltersRequest
                 | FiltersResponse
                 | FilterEvent
                 | PeerRequest
                 | PeerResponse
                 | FeeRequest
                 | FeeResponse
                 | IntroducePeer
                 | Reject
                 | Ping
                 | Pong

data Message = Message
  { messageMessageType :: MessageType
  , messageLength      :: Word32
  , messagePayload     :: [Word]
  }

data ScanBlocks = ScanBlocks
  { scanBlocksCurrency     :: Word
  , scanBlocksVersion      :: Word32
  , scanBlocksScanHeight   :: Word64
  , scanBlocksActualHeight :: Word64
  }

data VersionMessage = VersionMessage
  { versionMsgVersionNumber :: Word
  , versionMsgTime          :: Word64
  , versionMsgNonce         :: Word64
  , versionMsgCurrencies    :: Word32
  }