module Data.Ergo.Block(
    BlockVersion
  , Digest32(..)
  , ADDigest
  , Digest33(..)
  , ParamVotes(..)
  , BlockHeader(..)
  , hashHeaderBytes
  ) where

import Control.Monad
import Crypto.Hash (hashWith, Blake2b_256(..))
import Data.ByteString (ByteString)
import Data.Ergo.Autolykos
import Data.Ergo.Difficulty
import Data.Ergo.Modifier
import Data.Ergo.Vlq
import Data.Int
import Data.Persist
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import GHC.Generics

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base16 as B16

-- | Protocol version, for now always 1
type BlockVersion = Word8

-- | Hash of something length of 32 bytes
newtype Digest32 = Digest32 { unDigest32 :: ByteString }
  deriving (Generic, Show, Read, Eq)

instance Persist Digest32 where
  put = putByteString . unDigest32
  {-# INLINE put #-}
  get = fmap Digest32 $ getBytes 32
  {-# INLINE get #-}

-- | AVL+ tree digest of UTXO set (after the block). Extra byte with tree height
type ADDigest = Digest33

-- | Hash of something length of 33 bytes
newtype Digest33 = Digest33 { unDigest33 :: ByteString }
  deriving (Generic, Show, Read, Eq)

instance Persist Digest33 where
  put = putByteString . unDigest33
  {-# INLINE put #-}
  get = fmap Digest33 $ getBytes 33
  {-# INLINE get #-}

-- | Votes for changing system parameters. Each value represents index of parameter
-- either positive (for increasing of value) or negative (for decreasing of value)
--
-- TODO: define ADT for parameters.
data ParamVotes = ParamVotes !Int8 !Int8 !Int8
  deriving (Generic, Show, Read, Eq)

instance Persist ParamVotes where
  put (ParamVotes a b c) = put a >> put b >> put c
  get = ParamVotes <$> get <*> get <*> get
  {-# INLINE put #-}
  {-# INLINE get #-}

-- | Header of a block. It authenticates link to a previous block, other block sections
-- (transactions, UTXO set transformation proofs, extension), UTXO set, votes for parameters
-- to be changed and proof-of-work related data.
data BlockHeader = BlockHeader {
  version          :: !BlockVersion
, parentId         :: !ModifierId -- ^ id of a parent block header
, adProofsRoot     :: !Digest32 -- ^ digest of UTXO set transformation proofs
, transactionsRoot :: !Digest32 -- ^ Merkle tree digest of transactions in the block (BlockTransactions section)
, stateRoot        :: !ADDigest -- ^ AVL+ tree digest of UTXO set (after the block). Extra byte with tree height
, timestamp        :: !UTCTime -- ^ block generation time reported by a miner
, extensionRoot    :: !Digest32 -- ^ Merkle tree digest of the extension section of the block
, nBits            :: !Difficulty -- ^ difficulty encoded
, height           :: !Word32 -- ^ height of the block (genesis block height == 1)
, votes            :: !ParamVotes -- ^ votes for changing system parameters
, powSolution      :: !AutolykosSolution -- ^ solution for the proof-of-work puzzle
} deriving (Generic, Show, Read, Eq)

-- Header id is blake2b256 of its bytes
hashHeaderBytes :: ByteString -> ByteString
hashHeaderBytes = B16.encode . BA.convert . hashWith Blake2b_256

instance Persist BlockHeader where
  put BlockHeader{..} = do
    put version
    put parentId
    put adProofsRoot
    put transactionsRoot
    put stateRoot
    encodeVlq $ (floor . realToFrac . (* 1000) . utcTimeToPOSIXSeconds $ timestamp :: Word64)
    put nBits
    put extensionRoot
    encodeVlq height
    put votes
    when (version >= 2) $
      -- Block version V2 specific field, contains length of additional data
      put (0 :: Word8)
    put powSolution
  {-# INLINE put #-}

  get = do
    version          <- get
    parentId         <- get
    adProofsRoot     <- get
    transactionsRoot <- get
    stateRoot        <- get
    timestamp        <- (posixSecondsToUTCTime . (/ 1000) . fromIntegral <$> (decodeVlq :: Get Word64))
    nBits            <- get
    extensionRoot    <- get
    height           <- decodeVlq
    votes            <- get
    case version of
      1 -> do
          powSolution      <- get
          pure BlockHeader {..}
      2 -> do
          -- For block version >= 2, a new byte encodes length of possible new fields.
          -- If this byte > 0, we read new fields but do nothing, as semantics of the fields is not known.
          newFieldsSize :: Word8 <- get
          when (newFieldsSize > 0) $ do
            void $ getBytes (fromIntegral newFieldsSize)
          powSolution      <- unAutolykosV2 <$> get
          pure BlockHeader {..}
      _ -> fail ("Unsupported header version " <> show version)
  {-# INLINE get #-}
