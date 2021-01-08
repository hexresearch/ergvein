{-|
Module      : Data.Ergo.Protocol.Check
Description : Utils to generate checksums based on blake2b
Copyright   : (c) 2020 ATUM SOLUTIONS AG
License     : MIT
Maintainer  : ncrashed@protonmail.com
Stability   : experimental
Portability : POSIX
-}
module Data.Ergo.Protocol.Check(
    checkSum
  , validateSum
  ) where

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (Blake2b_256(..))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

import Debug.Trace

type CheckSum = ByteString

-- | Take 4 bytes from blake2b256 of payload
checkSum :: ByteString -> CheckSum
checkSum = BS.take 4 . convert . hashWith Blake2b_256

-- | Validate given payload check sum
validateSum :: CheckSum -> ByteString -> Bool
validateSum s = traceShow s . (s ==) . traceShowId . checkSum
