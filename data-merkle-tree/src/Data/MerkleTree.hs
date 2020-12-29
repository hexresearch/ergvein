module Data.MerkleTree (
  MerkleTree(..),
  MerkleRoot(..),
  MerkleNode(..),

  -- ** Constructors
  mkMerkleTree,
  mkRootHash,
  mkLeafRootHash,
  emptyHash,

  -- ** Merkle Proof
  MerkleProof(..),
  merkleProof,
  validateMerkleProof,

  -- ** Size
  mtRoot,
  mtSize,
  mtHash,
  mtHeight,
  mtWidth
) where

import Crypto.Hash (Digest, SHA3_256(..), hash)

import qualified Data.Serialize as S
import qualified Data.ByteArray as B
import qualified Data.ByteArray.Encoding as B
import qualified Data.ByteString as BS
import Data.Word
import GHC.Generics
import Data.Bits
import Control.DeepSeq

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | A merkle tree root.
newtype MerkleRoot a = MerkleRoot
  { getMerkleRoot :: BS.ByteString
  } deriving (Show, Read, Eq, Ord,  Generic, S.Serialize, NFData)

instance B.ByteArrayAccess (MerkleRoot a) where
  length (MerkleRoot bs) = B.length bs
  withByteArray (MerkleRoot bs) f = B.withByteArray bs f

-- | A merkle tree.
data MerkleTree a
  = MerkleEmpty
  | MerkleTree Word32 (MerkleNode a)
  deriving (Show, Read, Eq, Generic, S.Serialize, NFData)

data MerkleNode a
  = MerkleBranch {
      mRoot  :: MerkleRoot a
    , mLeft  :: MerkleNode a
    , mRight :: MerkleNode a
  }
  | MerkleLeaf {
      mRoot :: MerkleRoot a
    , mVal  :: a
  }
  deriving (Eq, Show, Read, Generic, S.Serialize, NFData)

instance Foldable MerkleTree where
  foldMap _ MerkleEmpty      = mempty
  foldMap f (MerkleTree _ n) = foldMap f n

  null MerkleEmpty = True
  null _           = False

  length MerkleEmpty      = 0
  length (MerkleTree s _) = fromIntegral s

instance Foldable MerkleNode where
  foldMap f x = case x of
    MerkleLeaf{mVal}            -> f mVal
    MerkleBranch{mLeft, mRight} ->
      foldMap f mLeft `mappend` foldMap f mRight

-- | Returns root of merkle tree.
mtRoot :: MerkleTree a -> MerkleRoot a
mtRoot MerkleEmpty      = emptyHash
mtRoot (MerkleTree _ x) = mRoot x

-- | Returns root of merkle tree root hashed.
mtHash :: MerkleTree a -> BS.ByteString
mtHash MerkleEmpty      = merkleHash ""
mtHash (MerkleTree _ x) = B.convert (mRoot x)

mtSize :: MerkleTree a -> Word32
mtSize MerkleEmpty      = 0
mtSize (MerkleTree s _) = s

emptyHash :: MerkleRoot a
emptyHash = MerkleRoot (merkleHash mempty)

-- | Merkle tree height
mtHeight :: Int -> Int
mtHeight ntx
  | ntx < 2 = 0
  | even ntx  = 1 + mtHeight (ntx `div` 2)
  | otherwise = mtHeight $ ntx + 1

-- | Merkle tree width
mtWidth
  :: Int -- ^ Number of transactions (leaf nodes).
  -> Int -- ^ Height at which we want to compute the width.
  -> Int -- ^ Width of the merkle tree.
mtWidth ntx h = (ntx + (1 `shiftL` h) - 1) `shiftR` h

-- | Return the largest power of two such that it's smaller than n.
powerOfTwo :: (Bits a, Num a) => a -> a
powerOfTwo n
   | n .&. (n - 1) == 0 = n `shiftR` 1
   | otherwise = go n
 where
    go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

mkLeaf :: BS.ByteString -> MerkleNode BS.ByteString
mkLeaf a =
  MerkleLeaf
  { mVal  = a
  , mRoot = mkLeafRootHash a
  }

mkLeafRootHash :: B.ByteArrayAccess a => a -> MerkleRoot a
mkLeafRootHash a = MerkleRoot $ merkleHash (BS.singleton 0 <> B.convert a)

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch a b =
  MerkleBranch
  { mLeft  = a
  , mRight = b
  , mRoot  = mkRootHash (mRoot a) (mRoot b)
  }

mkRootHash :: MerkleRoot a -> MerkleRoot a -> MerkleRoot a
mkRootHash (MerkleRoot l) (MerkleRoot r) = MerkleRoot $ merkleHash $ mconcat
  [ BS.singleton 1, B.convert l, B.convert r ]

-- | Smart constructor for 'MerkleTree'.
mkMerkleTree :: [BS.ByteString] -> MerkleTree BS.ByteString
mkMerkleTree [] = MerkleEmpty
mkMerkleTree ls = MerkleTree (fromIntegral lsLen) (go lsLen ls)
  where
    lsLen = length ls
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-------------------------------------------------------------------------------
-- Merkle Proofs
-------------------------------------------------------------------------------

newtype MerkleProof a = MerkleProof { getMerkleProof :: [ProofElem a] }
  deriving (Show, Read, Eq, Ord, Generic, S.Serialize, NFData)

data ProofElem a = ProofElem
  { nodeRoot    :: MerkleRoot a
  , siblingRoot :: MerkleRoot a
  , nodeSide    :: Side
  } deriving (Show, Read, Eq, Ord, Generic, S.Serialize, NFData)

data Side = L | R
  deriving (Show, Read, Eq, Ord, Generic, S.Serialize, NFData)

-- | Construct a merkle tree proof of inclusion
-- Walks the entire tree recursively, building a list of "proof elements"
-- that are comprised of the current node's root and it's sibling's root,
-- and whether it is the left or right sibling (this is necessary to determine
-- the order in which to hash each proof element root and it's sibling root).
-- The list is ordered such that the for each element, the next element in
-- the list is the proof element corresponding to the node's parent node.
merkleProof :: forall a. MerkleTree a -> MerkleRoot a -> MerkleProof a
merkleProof MerkleEmpty _ = MerkleProof []
merkleProof (MerkleTree _ rootNode) leafRoot = MerkleProof $ constructPath [] rootNode
  where
    constructPath :: [ProofElem a] -> MerkleNode a -> [ProofElem a]
    constructPath pElems (MerkleLeaf leafRoot' _)
      | leafRoot == leafRoot' = pElems
      | otherwise             = []
    constructPath pElems (MerkleBranch _ ln rn) = lPath ++ rPath
      where
        lProofElem = ProofElem (mRoot ln) (mRoot rn) L
        rProofElem = ProofElem (mRoot rn) (mRoot ln) R

        lPath = constructPath (lProofElem:pElems) ln
        rPath = constructPath (rProofElem:pElems) rn

-- | Validate a merkle tree proof of inclusion
validateMerkleProof :: forall a. MerkleProof a ->  MerkleRoot a -> MerkleRoot a -> Bool
validateMerkleProof (MerkleProof proofElems) treeRoot leafRoot =
    validate proofElems leafRoot
  where
    validate :: [ProofElem a] -> MerkleRoot a -> Bool
    validate [] proofRoot = proofRoot == treeRoot
    validate (pElem:pElems) proofRoot
      | proofRoot /= nodeRoot pElem = False
      | otherwise = validate pElems $ hashProofElem pElem

    hashProofElem :: ProofElem a -> MerkleRoot a
    hashProofElem (ProofElem pRoot sibRoot side) =
      case side of
        L -> mkRootHash pRoot sibRoot
        R -> mkRootHash sibRoot pRoot

-------------------------------------------------------------------------------
-- Hashing
-------------------------------------------------------------------------------

-- | Compute SHA-256 hash of a bytestring.
-- Maximum input size is (2^{64}-1)/8 bytes.
--
-- > Output size         : 256
-- > Internal state size : 1600
-- > Block size          : 1088
-- > Length size         : n/a
-- > Word size           : 64
-- > Rounds              : 24
sha256 :: BS.ByteString -> BS.ByteString
sha256 x = B.convertToBase B.Base16 (hash x :: Digest SHA3_256)

-- | Hash function to use for merkle tree
merkleHash :: BS.ByteString -> BS.ByteString
merkleHash = sha256
