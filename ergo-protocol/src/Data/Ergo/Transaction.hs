module Data.Ergo.Transaction
  (   TxInput (..)
    , TxDataInput (..)
    , Token (..)
    , Transaction (..)
    , ErgoBoxCandidate (..)
  )
where

import Data.ByteString (ByteString)
import Data.Ergo.Block (Digest32 (Digest32))
import Data.Map (Map)
import Data.Persist
import Data.Word (Word32, Word64)

data TxInput = TxInput
  { -- | Id of the box to spent
    boxId :: !Digest32,
    -- | Proof of spending correctness
    spendingProof :: !ByteString
  }
  deriving (Show, Read, Eq)

newtype TxDataInput = TxDataInput
  { -- | Id of the box to add into context (should be in UTXO)
    dataInput'boxId :: Digest32
  }
  deriving (Show, Read, Eq)

data Token = Token
  { -- Token id
    tokenId :: Digest32,
    -- Token amount
    tokenAmount :: Word64
  }
  deriving (Show, Read, Eq)

-- | Contains the same fields as `ErgoBox`, except if transaction id and index,
-- that will be calculated after full transaction formation.
data ErgoBoxCandidate = ErgoBoxCandidate
  { -- | Amount of money associated with the box
    boxValue :: !Word64,
    -- | Guarding script, which should be evaluated to true in order to open this box
    ergoTree :: !ByteString,
    -- | Secondary tokens the box contains
    tokens :: ![Token],
    -- | Additional registers the box can carry over
    additionalRegisters :: !ByteString,
    -- | Height when a transaction containing the box was created.
    -- This height is declared by user and should not exceed height of the block,
    -- containing the transaction with this box.
    creationHeight :: Word32
  }
  deriving (Show, Read, Eq)

data Transaction = Transaction
  { -- | Inputs, that will be spent by this transaction
    inputs :: ![TxInput],
    -- | Inputs, that are not going to be spent by transaction, but will be reachable from inputs
    -- scripts. `dataInputs` scripts will not be executed, thus their scripts costs are not
    -- included in transaction cost and they do not contain spending proofs.
    dataInputs :: ![TxDataInput],
    -- | Box candidates to be created by this transaction. Differ from ordinary ones in that
    -- they do not include transaction id and index.
    outputCandidates :: ![ErgoBoxCandidate]
  }
  deriving (Show, Read, Eq)

instance Persist Transaction where
  put = undefined
  {-# INLINE put #-}

  get = undefined
  {-# INLINE get #-}
