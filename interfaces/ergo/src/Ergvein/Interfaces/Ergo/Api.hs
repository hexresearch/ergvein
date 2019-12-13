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

import Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash (Digest32(..))
import Ergvein.Interfaces.Ergo.Scorex.Util.Package

data ErgoTransactionInput = ErgoTransactionInput {
  boxId :: !TransactionBoxId
    -- $ref: '#/components/schemas/TransactionBoxId'
, spendingProof :: !SpendingProof
    -- $ref: '#/components/schemas/SpendingProof'
, extension :: !(Properties SValue)
    -- type: object
    -- additionalProperties:
      -- $ref: '#/components/schemas/SValue'
    -- example:
      -- '1': 'a2aed72ff1b139f35d1ad2938cb44c9848a34d4dcfd6d8ab717ebde40a7304f2541cf628ffc8b5c496e6161eba3f169c6dd440704b1719e0'
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
  -- - boxId
  boxId :: !TransactionBoxId
    -- $ref: '#/components/schemas/TransactionBoxId'
, extension :: !(Properties SValue)
    -- type: object
    -- additionalProperties:
      -- $ref: '#/components/schemas/SValue'
    -- example:
      -- '1': 'a2aed72ff1b139f35d1ad2938cb44c9848a34d4dcfd6d8ab717ebde40a7304f2541cf628ffc8b5c496e6161eba3f169c6dd440704b1719e0'
}

-- | Spending proof for transaction input
data SpendingProof = SpendingProof {
  proofBytes :: !SpendingProofBytes
    -- $ref: '#/components/schemas/SpendingProofBytes'
, extension :: !(Properties SValue)
    -- type: object
    -- description: Variables to be put into context
    -- additionalProperties:
      -- $ref: '#/components/schemas/SValue'
    -- example:
      -- '1': 'a2aed72ff1b139f35d1ad2938cb44c9848a34d4dcfd6d8ab717ebde40a7304f2541cf628ffc8b5c496e6161eba3f169c6dd440704b1719e0'
}

data ErgoTransactionOutput = ErgoTransactionOutput {
  boxId :: !(Maybe TransactionBoxId)
    -- $ref: '#/components/schemas/TransactionBoxId'
, value :: !Word64
    -- description: Amount of Ergo token
    -- type: integer
    -- format: int64
    -- minimum: 0
    -- example: 147
, ergoTree :: !ErgoTree
    -- $ref: '#/components/schemas/ErgoTree'
, creationHeight :: Int32
    -- description: Height the output was created at
    -- type: integer
    -- format: int32
    -- example: 9149
, assets :: ![Asset]
    -- description: Assets list in the transaction
    -- type: array
    -- items:
      -- $ref: '#/components/schemas/Asset'
, additionalRegisters :: !Registers
    -- $ref: '#/components/schemas/Registers'
, transactionId :: !(Maybe TransactionId)
    -- $ref: '#/components/schemas/TransactionId'
, index :: !(Maybe Int32)
    -- description: Index in the transaction outputs
    -- type: integer
    -- format: int32
}

data ErgoTransaction = ErgoTransaction {
  tid :: !TransactionId -- field "id"
  -- $ref: '#/components/schemas/TransactionId'
, inputs :: ![ErgoTransactionInput]
  -- description: Many transaction inputs
  -- type: array
  -- items:
    -- $ref: '#/components/schemas/ErgoTransactionInput'
, dataInputs :: ![ErgoTransactionDataInput]
  -- description: Many transaction data inputs
  -- type: array
  -- items:
    -- $ref: '#/components/schemas/ErgoTransactionDataInput'
, outputs :: ![ErgoTransactionOutput]
  -- description: Many transaction outputs
  -- type: array
  -- items:
    -- $ref: '#/components/schemas/ErgoTransactionOutput'
, size :: !Int32
  -- description: Size in bytes
  -- type: integer
  -- format: int32
}

-- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/resources/api/openapi.yaml#L928
data NodeInfo = NodeInfo {
  name :: !Text
  -- name:
  -- type: string
  -- example: my-node-1
, appVersion :: !Text
  -- appVersion:
  -- type: string
  -- example: 0.0.1
, fullHeight :: !Int32
  -- fullHeight:
  -- type: integer
  -- format: int32
  -- description: Can be 'null' if state is empty (no full block is applied since node launch)
  -- minimum: 0
  -- example: 667
  -- nullable: true
, headersHeight :: !Int32
  -- headersHeight:
  -- type: integer
  -- format: int32
  -- description: Can be 'null' if state is empty (no header applied since node launch)
  -- minimum: 0
  -- example: 667
  -- nullable: true
, bestFullHeaderId :: !Text
  -- bestFullHeaderId:
  -- type: string
  -- description: Can be 'null' if no full block is applied since node launch
  -- nullable: true
  -- allOf:
  -- - $ref: '#/components/schemas/ModifierId'
, previousFullHeaderId :: !Text
  -- previousFullHeaderId:
  -- type: string
  -- description: Can be 'null' if no full block is applied since node launch
  -- nullable: true
  -- allOf:
  -- - $ref: '#/components/schemas/ModifierId'
, bestHeaderId :: !Text
  -- bestHeaderId:
  -- type: string
  -- description: Can be 'null' if no header applied since node launch
  -- nullable: true
  -- allOf:
  -- - $ref: '#/components/schemas/ModifierId'
, stateRoot :: !Text
  -- stateRoot:
  -- type: string
  -- nullable: true
  -- description: Can be 'null' if state is empty (no full block is applied since node launch)
  -- example: 'dab9da11fc216660e974842cc3b7705e62ebb9e0bf5ff78e53f9cd40abadd117'
, stateType :: !Text
  -- stateType:
  -- type: string
  -- enum:
  -- - digest
  -- - utxo
, stateVersion :: !Text
  -- stateVersion:
  -- description: Can be 'null' if no full block is applied since node launch
  -- type: string
  -- example: 'fab9da11fc216660e974842cc3b7705e62ebb9e0bf5ff78e53f9cd40abadd117'
  -- nullable: true
, isMining :: !Bool
  -- isMining:
  -- type: boolean
  -- example: true
, peersCount :: !Int32
  -- peersCount:
  -- type: integer
  -- description: Number of connected peers
  -- format: int32
  -- minimum: 0
  -- example: 327
, unconfirmedCount :: !Int32
  -- unconfirmedCount:
  -- description: Current unconfirmed transactions count
  -- type: integer
  -- format: int32
  -- minimum: 0
  -- maximum: 10000
  -- example: 327
, difficulty :: !Int32
  -- difficulty:
  -- type: integer
  -- format: int32
  -- minimum: 0
  -- nullable: true
  -- example: 667
  -- description: Difficulty on current bestFullHeaderId. Can be 'null' if no full block is applied since node launch
, currentTime :: !Int32
  -- currentTime:
  -- type: integer
  -- description: Current internal node time
  -- allOf:
  -- - $ref: '#/components/schemas/Timestamp'
, launchTime :: !Int32
  -- launchTime:
  -- type: integer
  -- description: Time when the node was started
  -- allOf:
  -- - $ref: '#/components/schemas/Timestamp'
, headersScore :: !Int32
  -- headersScore:
  -- type: integer
  -- description: Can be 'null' if no headers is applied since node launch
  -- nullable: true
, fullBlocksScore :: !Int32
  -- fullBlocksScore:
  -- type: integer
  -- description: Can be 'null' if no full block is applied since node launch
  -- nullable: true
, genesisBlockId :: !Text
  -- genesisBlockId:
  -- type: string
  -- description: Can be 'null' if genesis blocks is not produced yet
  -- nullable: true
  -- allOf:
  -- - $ref: '#/components/schemas/ModifierId'
, parameters :: !Parameters
  -- parameters:
  -- type: object
  -- description: current parameters
  -- $ref: '#/components/schemas/Parameters'
}

data Parameters = Parameters {
  height :: !Int32
    -- type: integer
    -- format: int32
    -- description: Height when current parameters were considered(not actual height). Can be '0' if state is empty
    -- minimum: 0
    -- example: 667
    -- nullable: false
, storageFeeFactor :: !Int32
    -- type: integer
    -- format: int32
    -- description: Storage fee coefficient (per byte per storage period ~4 years)
    -- minimum: 0
    -- example: 100000
    -- nullable: false
, minValuePerByte :: !Int32
    -- type: integer
    -- format: int32
    -- description: Minimum value per byte of an output
    -- minimum: 0
    -- example: 360
    -- nullable: false
, maxBlockSize :: !Int32
    -- type: integer
    -- format: int32
    -- description: Maximum block size (in bytes)
    -- minimum: 0
    -- example: 1048576
    -- nullable: false
, maxBlockCost :: !Int32
    -- type: integer
    -- format: int32
    -- description: Maximum cumulative computational complexity of input scipts in block transactions
    -- minimum: 0
    -- example: 104876
    -- nullable: false
, blockVersion :: !Int32
    -- $ref: '#/components/schemas/Version'
    -- nullable: false
, tokenAccessCost :: !Int32
    -- type: integer
    -- format: int32
    -- description: Validation cost of a single token
    -- minimum: 0
    -- example: 100
    -- nullable: false
, inputCost :: !Int32
    -- type: integer
    -- format: int32
    -- description: Validation cost per one transaction input
    -- minimum: 0
    -- example: 100
    -- nullable: false
, dataInputCost :: !Int32
    -- type: integer
    -- format: int32
    -- description: Validation cost per one data input
    -- minimum: 0
    -- example: 100
    -- nullable: false
, outputCost :: !Int32
    -- type: integer
    -- format: int32
    -- description: Validation cost per one transaction output
    -- minimum: 0
    -- example: 100
    -- nullable: false
}


-- | Token detail in the transaction
data Asset = Asset {
  tokenId :: !Digest32
    -- $ref: '#/components/schemas/Digest32'
, amount :: !Int64
    -- description: Amount of the token
    -- type: integer
    -- format: int64
    -- example: 1000
}

newtype Registers = Registers {
  -- description: Ergo box registers
  -- type: object
  unRegisters :: Properties SValue
  -- additionalProperties:
    -- $ref: '#/components/schemas/SValue'
  -- example:
    -- R4: '100204a00b08cd0336100ef59ced80ba5f89c4178ebd57b6c1dd0f3d135ee1db9f62fc634d637041ea02d192a39a8cc7a70173007301'
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

deriveJSON (A.defaultOptions { fieldLabelModifier = (\case { "tid" -> "id"; a -> a; }) }) ''ErgoTransaction
deriveJSON A.defaultOptions ''ErgoTransactionInput
deriveJSON A.defaultOptions ''ErgoTransactionDataInput
deriveJSON A.defaultOptions ''ErgoTransactionOutput
deriveJSON A.defaultOptions ''SpendingProof
deriveJSON A.defaultOptions ''Asset
deriveJSON A.defaultOptions ''Registers
