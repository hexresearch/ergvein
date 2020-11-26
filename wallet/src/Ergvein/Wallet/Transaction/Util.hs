module Ergvein.Wallet.Transaction.Util(
    checkAddr
  , checkAddrTx
  , checkAddrTxIn
  , checkAddrTxOut
  , checkOutIsOurs
  , checkOutSpent
  , checkTxIn
  , checkTxOut
  , countConfirmations
  , filterTxsForAddress
  , getConflictingTxs
  , getOutputByOutPoint
  , getOutputsByOutPoints
  , getSpentOutputs
  , getTxFee
  , getTxOutputsAmount
  , getUnspentOutputs
  , getUtxoUpdates
  , getUtxoUpdatesFromTxs
  , haveCommonInputs
  , haveHigherFee
  , haveSameInputs
  , inputSpendsOutPoint
  , markedReplaceable
  , replacesByFee
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Word
import Network.Haskoin.Transaction (Tx(..), TxIn(..), TxOut(..), OutPoint(..), txHash)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Keys
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native

import qualified Data.List                          as L
import qualified Data.Map.Strict                    as M
import qualified Data.Vector                        as V
import qualified Network.Haskoin.Address            as HA
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HK

checkAddr :: (HasTxStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
checkAddr ac tx = do
  bL <- traverse (flip checkAddrTx (getBtcTx tx)) ac
  pure $ L.or bL

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (HasTxStorage m, PlatformNatives) => EgvAddress -> Tx -> m Bool
checkAddrTx addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HK.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOut addr) (HK.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = L.foldr (||) False

-- | Checks given tx if there are some inputs containing given address.
checkAddrTxIn :: (HasTxStorage m, PlatformNatives) => EgvAddress -> Tx -> m Bool
checkAddrTxIn addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HK.txIn tx)
  pure $ concatResults checkTxInputsResults
  where concatResults = L.foldr (||) False

-- | Checks given tx if there are some outputs containing given address.
checkAddrTxOut :: (HasTxStorage m, PlatformNatives) => EgvAddress -> Tx -> m Bool
checkAddrTxOut addr tx = do
  checkTxOutputsResults <- traverse (checkTxOut addr) (HK.txOut tx)
  pure $ concatResults checkTxOutputsResults
  where concatResults = L.foldr (||) False

checkOutIsOurs :: (MonadIO m, PlatformNatives) => [EgvAddress] -> HK.TxOut -> m Bool
checkOutIsOurs addrs out = do
  results <- traverse (flip checkTxOut out) addrs
  pure $ L.any (== True) results

checkOutSpent :: [[HK.TxIn]] -> HK.OutPoint -> Bool
checkOutSpent inputs out = let results = (fmap . fmap) (inputSpendsOutPoint out) inputs in
  (L.any (== True)) $ fmap (L.any (== True)) results

-- | Checks given TxIn wheather it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxIn :: (HasTxStorage m, PlatformNatives) => EgvAddress -> TxIn -> m Bool
checkTxIn addr txIn = do
  let spentOutput = HK.prevOutput txIn
      spentTxHash = HK.outPointHash spentOutput
      spentOutputIndex = HK.outPointIndex spentOutput
  mtx <- getTxById $ hkTxHashToEgv spentTxHash
  case mtx of
    Nothing -> pure False
    Just ErgTx{} -> pure False -- TODO: impl for Ergo
    Just BtcTx{..} -> checkTxOut addr $ (HK.txOut getBtcTx) !! (fromIntegral spentOutputIndex)

-- | Checks given TxOut wheather it contains given address.
-- TODO: Pattern match(es) are non-exhaustive:
--       Patterns not matched:
--           (BtcAddress (HA.PubKeyAddress _)) _
--           (BtcAddress (HA.ScriptAddress _)) _
--           (ErgAddress _) _
checkTxOut :: (MonadIO m, PlatformNatives) => EgvAddress -> TxOut -> m Bool
checkTxOut (BtcAddress (HA.WitnessPubKeyAddress pkh)) txO = case HS.decodeOutputBS $ HK.scriptOutput txO of
  Left e -> do
    logWrite $ "Could not decode transaction output " <> (showt e)
    pure False
  Right output -> case output of
    HS.PayWitnessPKHash h -> if h == pkh then pure True else pure False
    _ -> pure False
checkTxOut (BtcAddress (HA.WitnessScriptAddress sh)) txO = case HS.decodeOutputBS $ HK.scriptOutput txO of
  Left e -> do
    logWrite $ "Could not decode transaction output " <> (showt e)
    pure False
  Right output -> case output of
    HS.PayWitnessScriptHash h -> if h == sh then pure True else pure False
    _ -> pure False
checkTxOut _ _ = pure False

countConfirmations :: BlockHeight -> Maybe BlockHeight -> Word64
countConfirmations _ Nothing = 0
countConfirmations currentHeight (Just confirmationHeight) = currentHeight - confirmationHeight + 1

-- | Filter txs for ones, relevant to an address
filterTxsForAddress :: (HasTxStorage m, PlatformNatives) => EgvAddress -> [Tx] -> m [Tx]
filterTxsForAddress addr txs = fmap catMaybes $ flip traverse txs $ \tx -> do
  b <- checkAddrTx addr tx
  pure $ if b then Just tx else Nothing

-- | Gets a list of groups of conflicting txs. Txs in the same group have at least one common input.
getConflictingTxs :: [EgvTx] -> [[TxId]]
getConflictingTxs txs = getConflicts <$> btcTxs
  where
    btcTxs = getBtcTx <$> txs
    getTxHash = hkTxHashToEgv . HK.txHash

    getConflicts :: HK.Tx -> [TxId]
    getConflicts tx = getTxHash <$> (L.filter (haveCommonInputs tx) (L.delete tx btcTxs))

getOutputByOutPoint :: (HasTxStorage m, PlatformNatives) => HK.OutPoint -> m (Maybe HK.TxOut)
getOutputByOutPoint HK.OutPoint{..} = do
  mtx <- getTxById $ hkTxHashToEgv outPointHash
  case mtx of
    Nothing -> pure Nothing
    Just ErgTx{} -> pure Nothing -- TODO: impl for Ergo
    Just BtcTx{..} -> pure $ Just $ (HK.txOut getBtcTx) !! (fromIntegral outPointIndex)

getOutputsByOutPoints :: (HasTxStorage m, PlatformNatives) => [HK.OutPoint] -> m [Maybe HK.TxOut]
getOutputsByOutPoints outPoints = traverse getOutputByOutPoint outPoints

-- | Gets spent output (they are inputs for a tx) for a given address from a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getSpentOutputs :: (HasTxStorage m, PlatformNatives) => Bool -> EgvAddress -> Tx -> m ([(OutPoint, Bool)])
getSpentOutputs c addr Tx{..} = fmap catMaybes $ flip traverse txIn $ \ti -> do
  b <- checkTxIn addr ti
  pure $ if b then Just (prevOutput ti, c) else Nothing

-- | Calculates tx fee. If any of parent txs were are found in the wallet storage then it returns Nothing.
getTxFee :: (HasTxStorage m, PlatformNatives) => Tx -> m (Maybe Word64)
getTxFee tx = do
  let inputs = HK.txIn tx
      outputs = HK.txOut tx
      outputsAmount = L.sum $ HK.outValue <$> outputs
  prevOutputs <- getOutputsByOutPoints $ HK.prevOutput <$> inputs
  let inputsAmount = L.sum $ HK.outValue <$> catMaybes prevOutputs
      result = if L.any isNothing prevOutputs then Nothing else Just $ inputsAmount - outputsAmount
  pure result

-- | Calculates amount of coins in tx outputs.
getTxOutputsAmount :: Tx -> Word64
getTxOutputsAmount tx = L.sum $ HK.outValue <$> outputs
  where outputs = HK.txOut tx

-- | Gets unspent output for a given address from a transaction
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUnspentOutputs :: (MonadIO m, PlatformNatives)
  => Maybe BlockHeight -> ScanKeyBox -> Tx -> m [(OutPoint, UtxoMeta)]
getUnspentOutputs c ScanKeyBox{..} tx = fmap catMaybes $ flip traverse (L.zip [0..] $ txOut tx) $ \(i,o) -> do
  b <- checkTxOut addr o
  let escript = HS.decodeOutputBS $ scriptOutput o
  either (\e -> logWrite $ "Failed to decode scriptOutput: " <> showt o <> ". Error: " <> showt e) (const $ pure ()) escript
  pure $ case (b, escript) of
    (True, Right scr) -> Just (OutPoint th i, UtxoMeta scanBox'index scanBox'purpose (outValue o) scr stat)
    _ -> Nothing
  where
    th = txHash tx
    stat = maybe (EUtxoReceiving Nothing) EUtxoSemiConfirmed c
    addr = egvXPubKeyToEgvAddress scanBox'key

-- | Construct UTXO update for a list of addresses based on a transaction
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUtxoUpdates :: (HasTxStorage m, PlatformNatives)
  => Maybe BlockHeight -> V.Vector ScanKeyBox -> Tx -> m BtcUtxoUpdate
getUtxoUpdates mheight boxes tx = do
  (unsps, sps) <- fmap V.unzip $ flip traverse boxes $ \box -> do
    let addr = egvXPubKeyToEgvAddress $ scanBox'key box
    unsp <- getUnspentOutputs mheight box tx
    sp   <- getSpentOutputs isConfirmed addr tx
    pure (unsp, sp)
  let unspentMap = M.fromList $ mconcat $ V.toList unsps
  pure (unspentMap, mconcat $ V.toList $ sps)
  where isConfirmed = maybe False (const True) mheight

-- | Construct UTXO update for an address and a batch of transactions
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUtxoUpdatesFromTxs :: (HasTxStorage m, PlatformNatives)
  => Maybe BlockHeight -> ScanKeyBox -> [Tx] -> m BtcUtxoUpdate
getUtxoUpdatesFromTxs mheight box txs = do
  (unsps, sps) <- fmap unzip $ flip traverse txs $ \tx -> do
    unsp <- getUnspentOutputs mheight box tx
    sp   <- getSpentOutputs isConfirmed addr tx
    pure (unsp, sp)
  let unspentMap = M.fromList $ mconcat unsps
  pure (unspentMap, mconcat sps)
  where
    isConfirmed = maybe False (const True) mheight
    addr = egvXPubKeyToEgvAddress $ scanBox'key box

haveCommonInputs :: Tx -> Tx -> Bool
haveCommonInputs tx1 tx2 = (not . L.null) $ (prevOutput <$> txIn tx1) `L.intersect` (prevOutput <$> txIn tx2)

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

haveSameInputs :: Tx -> Tx -> Bool
haveSameInputs tx1 tx2 = L.sort (prevOutput <$> txIn tx1) == L.sort (prevOutput <$> txIn tx2)

-- | Check given TxIn wheather it spends OutPoint.
inputSpendsOutPoint :: OutPoint -> TxIn -> Bool
inputSpendsOutPoint outPoint txIn = prevOutput txIn == outPoint

markedReplaceable :: Tx -> Bool
markedReplaceable tx = L.any (\input -> txInSequence input < (maxBound :: Word32) - 1) (txIn tx)

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
    (True, True, Nothing) -> pure $ Nothing
    (True, True, Just True) -> pure $ Just True
  where
    shareInputs = haveCommonInputs tx1 tx2
