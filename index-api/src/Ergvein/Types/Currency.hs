module Ergvein.Types.Currency where

import Data.Word
import Data.Text

-- | Supported currencies 
data Currency = BTC | ERGO

-- | Smallest amount of currency
type MoneyUnit = Word64

-- | Hexadecimal representation of transaction id
type TxId = Text

-- | Hexadecimal representation of transaction
type TxHexView = Text

-- | Number of blocks before current one, from the starting from Genesis block with height of zero
type BlockHeight = Word64

-- | Index of the transaction in block
type TxBlockIndex = Word

-- | Node in Merkle tree, hash of concatenated child nodes
type MerkleSum = Text

-- | Brunch
type TxMerkleProof = [MerkleSum]

-- | SHA256 hash of locking script with big-endian byte order, used to track transfers due inaccessibility
-- of transaction addresses when indexer scans blockchain
type PubKeyScriptHash = Text

-- | Hexadecimal representation of transaction hash
type TxHash = Text

