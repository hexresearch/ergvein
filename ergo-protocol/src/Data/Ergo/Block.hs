module Data.Ergo.Block(
    BlockHeader(..)
  ) where

import Data.Persist
import GHC.Generics

-- | Header of a block. It authenticates link to a previous block, other block sections
-- (transactions, UTXO set transformation proofs, extension), UTXO set, votes for parameters
-- to be changed and proof-of-work related data.
data BlockHeader = BlockHeader {

} deriving (Generic, Show, Read, Eq)
