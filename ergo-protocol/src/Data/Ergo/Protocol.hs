{-|
Module      : Data.Ergo.Protocol
Description : Data types for messages for P2P protocol for Ergo platform.
Copyright   : (c) 2020 ATUM SOLUTIONS AG
License     : MIT
Maintainer  : ncrashed@protonmail.com
Stability   : experimental
Portability : POSIX

The module contains only data types and serialization and deserialization code
for network messages in peer to peer protocol that is used in Ergo cryptocurrency.

Types are kept close to binary representation for fast processing.
-}
module Data.Ergo.Protocol(
    Network(..)
  , TestnetMessage(..)
  , MainnetMessage(..)
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
  , syncInfoId
  , decodeMessage
  , encodeMessage
  ) where

import Data.Ergo.Protocol.Decoder
import Data.Ergo.Protocol.Encoder
import Data.Ergo.Protocol.Types
import Data.Persist
import GHC.Generics

newtype TestnetMessage = TestnetMessage { unTestnetMessage :: Message }
  deriving (Generic, Show, Read, Eq)

newtype MainnetMessage = MainnetMessage { unMainnetMessage :: Message }
  deriving (Generic, Show, Read, Eq)

instance Persist Handshake where
  put = handshakeEncoder
  get = handshakeParser

instance Persist TestnetMessage where
  put = messageEncoder Testnet . unTestnetMessage
  get = TestnetMessage <$> messageParser Testnet

instance Persist MainnetMessage where
  put = messageEncoder Mainnet . unMainnetMessage
  get = MainnetMessage <$> messageParser Mainnet
