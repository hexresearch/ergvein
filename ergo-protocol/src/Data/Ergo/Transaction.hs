module Data.Ergo.Transaction
  (   TxInput (..)
    , TxDataInput (..)
    , Token (..)
    , Transaction (..)
    , ErgoBox (..)
    , ErgoBoxCandidate (..)
  )
where

import Data.ByteString (ByteString)
import Data.Ergo.Block (Digest32 (Digest32))
import Data.Persist
import Data.Word (Word16, Word32, Word64)

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
    tokenId :: !Digest32,
    -- Token amount
    tokenAmount :: !Word64
  }
  deriving (Show, Read, Eq)

-- Box (aka coin, or an unspent output) is a basic concept of a UTXO-based cryptocurrency.
-- In Bitcoin, such an object is associated with some monetary value (arbitrary,
-- but with predefined precision, so we use integer arithmetic to work with the value),
-- and also a guarding script (aka proposition) to protect the box from unauthorized opening.
--
-- In other way, a box is a state element locked by some proposition (ErgoTree).
--
-- In Ergo, box is just a collection of registers, some with mandatory types and semantics,
-- others could be used by applications in any way.
-- We add additional fields in addition to amount and proposition~(which stored in the registers R0 and R1).
-- Namely, register R2 contains additional tokens (a sequence of pairs (token identifier, value)).
-- Register R3 contains height specified by user (protocol checks if it was <= current height when
-- transaction was accepted) and also transaction identifier and box index in the transaction outputs.
-- Registers R4-R9 are free for arbitrary usage.
--
-- A transaction is unsealing a box. As a box can not be open twice, any further valid transaction
-- can not be linked to the same box.
data ErgoBox = ErgoBox
  { -- | Amount of money associated with the box
    ergoBox'boxValue :: !Word64,
    -- | Guarding script, which should be evaluated to true in order to open this box
    ergoBox'ergoTree :: !ByteString,
    -- | Secondary tokens the box contains
    ergoBox'tokens :: ![Token],
    -- | Additional registers the box can carry over
    ergoBox'additionalRegisters :: !ByteString,
    -- | Height when a transaction containing the box was created.
    -- This height is declared by user and should not exceed height of the block,
    -- containing the transaction with this box.
    ergoBox'creationHeight :: !Word32,
    -- | Id of transaction which created the box
    ergoBox'transactionId :: !Digest32,
    -- | Number of box (from 0 to total number of boxes the transaction with transactionId created - 1)
    ergoBox'index :: !Word16
  }
  deriving (Show, Read, Eq)

-- | Contains the same fields as `ErgoBox`, except if transaction id and index,
-- that will be calculated after full transaction formation.
data ErgoBoxCandidate = ErgoBoxCandidate
  { -- | Amount of money associated with the box
    ergoBoxCandidate'boxValue :: !Word64,
    -- | Guarding script, which should be evaluated to true in order to open this box
    ergoBoxCandidate'ergoTree :: !ByteString,
    -- | Secondary tokens the box contains
    ergoBoxCandidate'tokens :: ![Token],
    -- | Additional registers the box can carry over
    ergoBoxCandidate'additionalRegisters :: !ByteString,
    -- | Height when a transaction containing the box was created.
    -- This height is declared by user and should not exceed height of the block,
    -- containing the transaction with this box.
    ergoBoxCandidate'creationHeight :: !Word32
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
