module Ergvein.Types.Transaction where

import Data.Text
import Data.Word
import Ergvein.Types.Currency

-- | Hexadecimal representation of transaction id
type TxId = Text

-- | Hexadecimal representation of transaction
type TxHexView = Text

-- | Number of blocks before current one, from the starting from Genesis block with height of zero
type BlockHeight = Word64

-- | Hash of block (usually header only) that identifies block.
type BlockHash = Text 

-- | Index of the transaction in block
type TxBlockIndex = Word

-- | Node in Merkle tree, hash of concatenated child nodes
type MerkleSum = Text

-- | Brunch of MerkleSums in Merkle tree, for transaction validation, deepest MerkleSum first
type TxMerkleProof = [MerkleSum]

-- | Fee included with transaction as price for processing transaction by miner
type TxFee = MoneyUnit

-- | SHA256 hash of locking script with big-endian byte order, used to track transfers due inaccessibility
-- of transaction addresses when indexer scans blockchain
type PubKeyScriptHash = Text

-- | Hexadecimal representation of transaction hash
type TxHash = Text

-- | Index of the UTXO
type TxOutIndex = Word