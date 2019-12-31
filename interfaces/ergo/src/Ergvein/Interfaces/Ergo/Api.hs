module Ergvein.Interfaces.Ergo.Api where

-- Models was taken from:
-- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/resources/api/openapi.yaml#L1057-L1136

import Data.Aeson as A
import Data.ByteString
import Data.Int
import Data.HashMap.Strict
import Data.String
import Data.Text (Text)
import Data.Word

import Ergvein.Aeson

import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Mining.AutolykosSolution
import Ergvein.Interfaces.Ergo.Mining.Difficulty.RequiredDifficulty
import Ergvein.Interfaces.Ergo.Modifiers.History.ModifierType
import Ergvein.Interfaces.Ergo.NodeView.History.ErgoHistory
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Interfaces.Ergo.Scorex.Crypto.Authds
import Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash (Digest32(..))
import Ergvein.Interfaces.Ergo.Scorex.Util.Package

data ErgoTransactionInput = ErgoTransactionInput {
  boxId :: !TransactionBoxId
, spendingProof :: !SpendingProof
, extension :: ! (Maybe (Properties SValue))
}

----------------------------------------

newtype TransactionBoxId = TransactionBoxId { unTransactionBoxId :: ByteString }
  deriving (Eq)

instance Show TransactionBoxId where
    show = show . toHex . unTransactionBoxId

instance IsString TransactionBoxId where
    fromString = TransactionBoxId . fromHex . fromString

instance ToJSON TransactionBoxId where
  toJSON = String . toHex . unTransactionBoxId
  {-# INLINE toJSON #-}

instance FromJSON TransactionBoxId where
  parseJSON = withText "TransactionBoxId" $
    either fail (pure . TransactionBoxId) . fromHexTextEither
  {-# INLINE parseJSON #-}

----------------------------------------

data ErgoTransactionDataInput = ErgoTransactionDataInput {
-- required:
  boxId :: !TransactionBoxId
, extension :: !(Maybe (Properties SValue))
}

-- | Spending proof for transaction input
data SpendingProof = SpendingProof {
  proofBytes :: !SpendingProofBytes
, extension :: !(Properties SValue)
}

data ErgoTransactionOutput = ErgoTransactionOutput {
  boxId :: !(Maybe TransactionBoxId)
, value :: !Word64
, ergoTree :: !ErgoTree
, creationHeight :: !Int32
, assets :: ![Asset]
, additionalRegisters :: !Registers
, transactionId :: !(Maybe TransactionId)
, index :: !(Maybe Int32)
}

data ErgoTransaction = ErgoTransaction {
  transactionId :: !TransactionId -- field "id"
, inputs :: ![ErgoTransactionInput]
, dataInputs :: ![ErgoTransactionDataInput]
, outputs :: ![ErgoTransactionOutput]
, size :: !Int32
}

-- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/resources/api/openapi.yaml#L928
data NodeInfo = NodeInfo {
  name :: !Text
, appVersion :: !Text
, fullHeight :: !Word32
, headersHeight :: !Word32
, bestFullHeaderId :: !Text
, previousFullHeaderId :: !Text
, bestHeaderId :: !Text
, stateRoot :: !Text
, stateType :: !Text
, stateVersion :: !Text
, isMining :: !Bool
, peersCount :: !Word32
, unconfirmedCount :: !Word32
, difficulty :: !Word32
, currentTime :: !Int32
, launchTime :: !Int32
, headersScore :: !Int32
, fullBlocksScore :: !Int32
, genesisBlockId :: !Text
, parameters :: !Parameters
}

data Parameters = Parameters {
  height :: !Word32
, storageFeeFactor :: !Word32
, minValuePerByte :: !Word32
, maxBlockSize :: !Word32
, maxBlockCost :: !Word32
, blockVersion :: !Int32
, tokenAccessCost :: !Word32
, inputCost :: !Word32
, dataInputCost :: !Word32
, outputCost :: !Word32
}

-- | Block with header and transactions
data FullBlock = FullBlock {
  header :: !BlockHeader
, blockTransactions :: !BlockTransactions
, adProofs :: !BlockADProofs
, extension :: !Extension
, size :: !Int32
}

data BlockHeader = BlockHeader {
  headerId :: !ModifierId
, timestamp :: !Timestamp
, version :: !Version
, adProofsRoot :: !Digest32
, stateRoot :: !ADDigest
, transactionsRoot :: !Digest32
, nBits :: !NBits
, extensionHash :: !Digest32
, powSolutions :: !AutolykosSolution
, height :: !Word32
, difficulty :: !Difficulty
, parentId :: !ModifierId
, votes :: !Votes
, size :: !(Maybe Int32)
, extensionId :: !(Maybe ModifierId)
, transactionsId :: !(Maybe ModifierId)
, adProofsId :: !(Maybe ModifierId)
}

headerFromApi :: BlockHeader -> Header
headerFromApi BlockHeader {..} = Header {
    version             = version
  , parentId            = parentId
  , adProofsRoot        = AdProofsRoot adProofsRoot
  , transactionsRoot    = TransactionsRoot transactionsRoot
  , stateRoot           = stateRoot
  , timestamp           = timestamp
  , extensionRoot       = ExtensionRoot extensionHash
  , nBits               = nBits
  , height              = Height height
  , votes               = votes
  , powSolution         = powSolutions
  }

data BlockTransactions = BlockTransactions {
  headerId :: !ModifierId
, transactions :: ![ErgoTransaction]
, size :: !Word32
}

data Extension = Extension {
  headerId :: !ModifierId
, digest :: !Digest32
, fields :: ![[HexJSON]]
}

data BlockADProofs = BlockADProofs {
  headerId :: !ModifierId
, proofBytes :: !SerializedAdProof
, digest :: !Digest32
, size :: !Int32
}

-- | Token detail in the transaction
data Asset = Asset {
  tokenId :: !Digest32
, amount :: !Int64
}

newtype Registers = Registers {
  unRegisters :: Properties SValue
}

type Properties a = HashMap Text a

----------------------------------------

-- | Base-16 encoded serialized Sigma-state value
newtype SValue = SValue { unSValue :: ByteString }
  deriving (Eq)

instance Show SValue where
    show = show . toHex . unSValue

instance IsString SValue where
    fromString = SValue . fromHex . fromString

instance ToJSON SValue where
  toJSON = String . toHex . unSValue
  {-# INLINE toJSON #-}

instance FromJSON SValue where
  parseJSON = withText "SValue" $
    either fail (pure . SValue) . fromHexTextEither
  {-# INLINE parseJSON #-}

----------------------------------------

-- | Base16-encoded transaction id bytes

newtype TransactionId = TransactionId { unTransactionId :: ByteString }
  deriving (Eq)

instance Show TransactionId where
    show = show . toHex . unTransactionId

instance IsString TransactionId where
    fromString = TransactionId . fromHex . fromString

instance ToJSON TransactionId where
  toJSON = String . toHex . unTransactionId
  {-# INLINE toJSON #-}

instance FromJSON TransactionId where
  parseJSON = withText "TransactionId" $
    either fail (pure . TransactionId) . fromHexTextEither
  {-# INLINE parseJSON #-}

----------------------------------------

-- | Base16-encoded spending proofs
newtype SpendingProofBytes = SpendingProofBytes { unSpendingProofBytes :: ByteString }
  deriving (Eq)

instance Show SpendingProofBytes where
    show = show . toHex . unSpendingProofBytes

instance IsString SpendingProofBytes where
    fromString = SpendingProofBytes . fromHex . fromString

instance ToJSON SpendingProofBytes where
  toJSON = String . toHex . unSpendingProofBytes
  {-# INLINE toJSON #-}

instance FromJSON SpendingProofBytes where
  parseJSON = withText "SpendingProofBytes" $
    either fail (pure . SpendingProofBytes) . fromHexTextEither
  {-# INLINE parseJSON #-}

----------------------------------------

-- | Base16-encoded ergo tree bytes
newtype ErgoTree = ErgoTree { unErgoTree :: ByteString }
  deriving (Eq)

instance Show ErgoTree where
    show = show . toHex . unErgoTree

instance IsString ErgoTree where
    fromString = ErgoTree . fromHex . fromString

instance ToJSON ErgoTree where
  toJSON = String . toHex . unErgoTree
  {-# INLINE toJSON #-}

instance FromJSON ErgoTree where
  parseJSON = withText "ErgoTree" $
    either fail (pure . ErgoTree) . fromHexTextEither
  {-# INLINE parseJSON #-}

----------------------------------------

-- | Base16-encoded ad proofs
newtype SerializedAdProof = SerializedAdProof { unSerializedAdProof :: ByteString }
  deriving (Eq)

instance Show SerializedAdProof where
    show = show . toHex . unSerializedAdProof

instance IsString SerializedAdProof where
    fromString = SerializedAdProof . fromHex . fromString

instance ToJSON SerializedAdProof where
  toJSON = String . toHex . unSerializedAdProof
  {-# INLINE toJSON #-}

instance FromJSON SerializedAdProof where
  parseJSON = withText "SerializedAdProof" $
    either fail (pure . SerializedAdProof) . fromHexTextEither
  {-# INLINE parseJSON #-}

----------------------------------------

deriveJSON (A.defaultOptions { fieldLabelModifier = (\case { "transactionId" -> "id"; a -> a; }) }) ''ErgoTransaction
deriveJSON A.defaultOptions ''ErgoTransactionInput
deriveJSON A.defaultOptions ''ErgoTransactionDataInput
deriveJSON A.defaultOptions ''ErgoTransactionOutput
deriveJSON A.defaultOptions ''SpendingProof
deriveJSON A.defaultOptions ''FullBlock
deriveJSON (A.defaultOptions { fieldLabelModifier = (\case { "headerId" -> "id"; a -> a; }) }) ''BlockHeader
deriveJSON A.defaultOptions ''BlockTransactions
deriveJSON A.defaultOptions ''Extension
deriveJSON A.defaultOptions ''BlockADProofs
deriveJSON A.defaultOptions ''Asset
deriveJSON unwrapUnaryOptions ''Registers
