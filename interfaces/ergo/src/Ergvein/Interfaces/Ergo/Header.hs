module Ergvein.Interfaces.Ergo.Header where

-----------------------------------------------------------------------------
-- | The port of:
-- ergo/src/main/scala/org/ergoplatform/modifiers/history/Header.scala
-----------------------------------------------------------------------------

import Data.Serialize                     as S (Serialize (..), decode, encode, get, put)
import Data.Serialize.Get                 as S
import Data.Serialize.Put                 as S

data Header = Header
-- {
                  -- version: Version,
                  -- override val parentId: ModifierId,
                  -- ADProofsRoot: Digest32,
                  -- stateRoot: ADDigest, //33 bytes! extra byte with tree height here!
                  -- transactionsRoot: Digest32,
                  -- timestamp: Timestamp,
                  -- nBits: Long, //actually it is unsigned int
                  -- height: Int,
                  -- extensionRoot: Digest32,
                  -- powSolution: AutolykosSolution,
                  -- votes: Array[Byte], //3 bytes
-- }

instance Serialize Header where
    put bn = do
      undefined
    get = do
      undefined
