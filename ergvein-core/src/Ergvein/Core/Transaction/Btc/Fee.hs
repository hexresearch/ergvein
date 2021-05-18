{-# OPTIONS_GHC -Wall #-}

module Ergvein.Core.Transaction.Btc.Fee(
    weightUnitsToVBytes
  , calcTxVsize
  , guessTxFee
  , guessTxVsize
  , BtcOutputType
  , BtcInputType
  , getTxFee
  , markedReplaceable
  , replacesByFee
  , haveHigherFee
  , isDust
) where

import Control.Monad (forM_)
import Data.Maybe (catMaybes, isNothing)
import Data.Ratio ((%))
import Data.Serialize
  ( Put,
    Serialize (put),
    encode,
    putByteString,
    runPut,
  )
import Data.Word (Word32, Word64)
import Ergvein.Core.Store.Monad
import Ergvein.Core.Transaction.Btc.Common
import Ergvein.Types.Address
import Ergvein.Types.Utxo.Btc
import Network.Haskoin.Network (VarInt (VarInt), putVarInt)
import Network.Haskoin.Transaction (Tx (..), TxIn (..), TxOut (..), WitnessData)
import Sepulcas.Native

import qualified Data.ByteString                    as B
import qualified Data.List                          as L

-- Min feerate for defining dust. Has units of Sat/vbyte.
dustRelayFee :: Int
dustRelayFee = 3

-- 4 weight units equals to one virtual byte
witnessScaleFactor :: Int
witnessScaleFactor = 4

isSegwit :: TxOut -> Bool
isSegwit txOut = case getBtcOutputType txOut of
  Just BtcP2WPKH -> True
  Just BtcP2WSH -> True
  _ -> False

getDustThreshold :: TxOut -> Int
getDustThreshold txOut = dustRelayFee * inOutSize
  where
    inOutSize :: Int
    inOutSize = if isSegwit txOut
      then getSerializedSize txOut + (32 + 4 + 1 + (107 `div` witnessScaleFactor) + 4)
      else getSerializedSize txOut + (32 + 4 + 1 + 107 + 4)

isDust :: TxOut -> Bool
isDust txOut = fromIntegral (outValue txOut) < getDustThreshold txOut

weightUnitsToVBytes :: Int -> Int
weightUnitsToVBytes wu = ceiling $ wu % witnessScaleFactor

getSerializedSize :: Serialize a => a -> Int
getSerializedSize = B.length . encode

calcTxVsize :: Tx -> Int
calcTxVsize tx
  | null (txWitness tx) = calcLegacyTxVsize tx
  | otherwise = calcWitnessTxVsize tx

calcLegacyTxVsize :: Tx -> Int
calcLegacyTxVsize = getSerializedSize

-- From haskoin-core
putInOut :: Tx -> Put
putInOut tx = do
  putVarInt $ length (txIn tx)
  forM_ (txIn tx) put
  putVarInt $ length (txOut tx)
  forM_ (txOut tx) put

-- | Witness data serializer.
putWitnessData :: WitnessData -> Put
putWitnessData = mapM_ putWitnessStack
  where
    putWitnessStack ws = do
      putVarInt $ length ws
      mapM_ putWitnessStackItem ws
    putWitnessStackItem bs = do
      putVarInt $ B.length bs
      putByteString bs

calcWitnessTxVsize :: Tx -> Int
calcWitnessTxVsize tx = weightUnitsToVBytes $ overhead + inOut + witnessData
  where
    overhead = 34 -- version 16 WU + marker 1 WU + flag 1 WU + locktime 16 WU
    inOut = (* witnessScaleFactor) $ fromIntegral $ B.length $ runPut $ putInOut tx
    witnessData = fromIntegral $ B.length $ runPut $ putWitnessData $ txWitness tx

type BtcOutputType = BtcAddressType

type BtcInputType = BtcAddressType

-- | Estimate tranasction fee to pay based on transaction virtual size estimation.
guessTxFee :: Word64 -> [BtcOutputType] -> [BtcInputType] -> Word64
guessTxFee vbyteFee outTypes inTypes =
  vbyteFee * fromIntegral (guessTxVsize inTypes outTypes)

-- | Returns input size in weight units including witness data
-- for P2SH, P2WSHInP2SH and P2WSH inputs we assume 1 pubkey and 1 signature
-- TODO: fix for more complex inputs
getInputWeight :: BtcInputType -> Word64
getInputWeight = \case
  BtcP2PKH -> 592
  BtcP2SH -> 612
  BtcP2WPKH -> 271
  BtcP2WSH -> 272
  -- BtcP2WPKHInP2SH -> 364
  -- BtcP2WSHInP2SH -> 412
  -- BtcP2TR -> 229
  _ -> error "getInputWeight: failed to calculate input weight"

-- | Returns putput size in weight units
getOutputWeight :: BtcOutputType -> Word64
getOutputWeight = \case
  BtcP2PKH -> 136
  BtcP2SH -> 128
  BtcP2WPKH -> 124
  BtcP2WSH -> 172
  -- BtcP2WPKHInP2SH -> 128
  -- BtcP2WSHInP2SH -> 128
  -- BtcP2TR -> 172
  _ -> error "getOutputWeight: failed to calculate output weight"

-- | Computes an upper bound on the virtual size of a transaction based on
-- inputs and outputs sets of the transaction.
guessTxVsize ::
     [BtcInputType]
  -> [BtcOutputType]
  -> Int
guessTxVsize inputs outputs =
  weightUnitsToVBytes $ fromIntegral $ sum (getInputWeight <$> inputs) + sum (getOutputWeight <$> outputs) + overhead
  where
    varIntSize = B.length . encode . VarInt . fromIntegral
    segWitInputs = filter (\x -> x /= BtcP2PKH && x /= BtcP2SH) inputs
    witnessOverheadWeight = if null segWitInputs then 0 else 2 + varIntSize (length segWitInputs)
    overhead = fromIntegral $ (witnessScaleFactor * (8 + varIntSize (length inputs) +  varIntSize (length outputs))) + witnessOverheadWeight

-- | Calculates tx fee. If any of the parent txs was not found in the wallet storage then it returns Nothing.
getTxFee :: (HasTxStorage m, PlatformNatives) => Tx -> m (Maybe Word64)
getTxFee tx = do
  let inputs = txIn tx
      outputs = txOut tx
      outputsAmount = L.sum $ outValue <$> outputs
  prevOutputs <- getOutputsByOutPoints $ prevOutput <$> inputs
  let inputsAmount = L.sum $ outValue <$> catMaybes prevOutputs
      result = if L.any isNothing prevOutputs then Nothing else Just $ inputsAmount - outputsAmount
  pure result

markedReplaceable :: Tx -> Bool
markedReplaceable tx = L.any (\input -> txInSequence input < (maxBound :: Word32) - 1) (txIn tx)

-- | Returns Just True if tx1 has higher fee than tx2, Just False if tx2 has higher fee than tx1.
-- If it was not possible to calculate the fee for any of the transactions
-- and transactions have different sets of inputs, returns Nothing.
haveHigherFee :: (HasTxStorage m, PlatformNatives) => Tx -> Tx -> m (Maybe Bool)
haveHigherFee tx1 tx2
  | haveSameInputs tx1 tx2 = pure $ Just $ getTxOutputsAmount tx1 < getTxOutputsAmount tx2
  | otherwise = do
      mFee1 <- getTxFee tx1
      mFee2 <- getTxFee tx2
      case (mFee1, mFee2) of
        (Just fee1, Just fee2) -> pure $ Just $ fee1 > fee2
        (_, _) -> pure Nothing

-- | Returns Just True if it is known for sure that tx1 replaces tx2 by fee.
-- Returns Just False if it is known for sure that tx1 not replaces tx2 by fee.
-- Returns Nothing if tx1 and tx2 have common inputs but we don't know which of txs have higher fee.
replacesByFee :: (HasTxStorage m, PlatformNatives) => Tx -> Tx -> m (Maybe Bool)
replacesByFee tx1 tx2 = do
  mHigherFee <- haveHigherFee tx1 tx2
  case (markedReplaceable tx2, shareInputs, mHigherFee) of
    (False, _, _) -> pure $ Just False
    (_, False, _) -> pure $ Just False
    (True, True, Just False) -> pure $ Just False
    (True, True, Nothing) -> pure Nothing
    (True, True, Just True) -> pure $ Just True
  where
    shareInputs = haveCommonInputs tx1 tx2
