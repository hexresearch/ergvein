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
  , Message(..)
  , ProtoVer(..)
  , IP(..)
  , NetAddr(..)
  , Handshake(..)
  , handshakeTimeout
  , magicBytes
  ) where

import Data.Ergo.Protocol.Types
import Data.Ergo.Protocol.Encoder
import Data.Ergo.Protocol.Decoder
