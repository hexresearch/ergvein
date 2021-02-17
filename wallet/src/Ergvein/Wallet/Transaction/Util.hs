module Ergvein.Wallet.Transaction.Util(
  -- Generalized functions
    checkAddr
  , checkAddrTx
  , countConfirmations
  -- Bitcoin functions
  , checkAddrTxInBtc
  , checkAddrTxOutBtc
  , checkOutIsOursBtc
  , filterTxsForAddressBtc
  , checkOutSpent
  , getConflictingTxs
  , isDirectChildTxOf
  , getChildTxs
  , getReplacedTxs
  , getPossiblyReplacedTxs
  , getOutputByOutPoint
  , getOutputsByOutPoints
  , getSpentOutputsBtc
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
  , buildAddrTxRbf
  -- Ergo functions
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector (Vector)
import Data.Word
import Network.Haskoin.Transaction (Tx(..), TxIn(..), TxOut(..), OutPoint(..), txHash)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Keys
import Ergvein.Types.Network (Network)
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Types.Utxo.Btc
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native

import qualified Data.ByteString                    as B
import qualified Data.List                          as L
import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Network.Haskoin.Address            as HA
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HK
import qualified Network.Haskoin.Util               as HU

checkAddr :: (HasTxStorage m, PlatformNatives) => [EgvAddress] -> EgvTx -> m Bool
checkAddr addrs tx = do
  bL <- traverse (checkAddrTx tx) addrs
  pure $ L.or bL

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (HasTxStorage m, PlatformNatives) => EgvTx -> EgvAddress -> m Bool
checkAddrTx (TxBtc (BtcTx tx _)) (BtcAddress addr) = checkAddrTxBtc tx addr
checkAddrTx (TxErg (ErgTx tx _)) (ErgAddress addr) = checkAddrTxErg tx addr
checkAddrTx _ _ = pure False

checkAddrTxBtc :: (HasTxStorage m, PlatformNatives) => BtcTxRaw -> BtcAddress -> m Bool
checkAddrTxBtc tx addr = do
  checkTxInputsResults <- traverse (checkTxInBtc addr) (HK.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOutBtcLog addr) (HK.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = L.foldr (||) False

checkAddrTxErg :: (HasTxStorage m, PlatformNatives) => ErgTxRaw -> ErgAddress -> m Bool
checkAddrTxErg = undefined

-- | Checks given tx if there are some inputs containing given address.
checkAddrTxInBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> BtcTxRaw -> m Bool
checkAddrTxInBtc addr tx = do
  checkTxInputsResults <- traverse (checkTxInBtc addr) (HK.txIn tx)
  pure $ or checkTxInputsResults

-- | Checks given tx if there are some outputs containing given address.
checkAddrTxOutBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> BtcTxRaw -> m Bool
checkAddrTxOutBtc addr tx = do
  checkTxOutputsResults <- traverse (checkTxOutBtcLog addr) (HK.txOut tx)
  pure $ or checkTxOutputsResults

checkOutIsOursBtc :: (MonadIO m, PlatformNatives) => [BtcAddress] -> HK.TxOut -> m Bool
checkOutIsOursBtc addrs out = do
  results <- traverse (flip checkTxOutBtcLog out) addrs
  pure $ L.any (== True) results

checkOutSpent :: [[HK.TxIn]] -> HK.OutPoint -> Bool
checkOutSpent inputs out = let results = (fmap . fmap) (inputSpendsOutPoint out) inputs in
  or $ fmap or results

-- | Checks given TxIn whether it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxInBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> TxIn -> m Bool
checkTxInBtc addr txIn = do
  let spentOutput = HK.prevOutput txIn
      spentTxHash = HK.outPointHash spentOutput
      spentOutputIndex = HK.outPointIndex spentOutput
  mtx <- getTxById $ hkTxHashToEgv spentTxHash
  case mtx of
    Nothing -> pure False
    Just (TxErg _) -> pure False -- TODO: impl for Ergo
    Just (TxBtc BtcTx{..}) -> checkTxOutBtcLog addr $ HK.txOut getBtcTx !! fromIntegral spentOutputIndex

decodeBtcOutHelper :: TxOut -> Either Text HS.ScriptOutput
decodeBtcOutHelper txOut = case HS.decodeOutputBS $ HK.scriptOutput txOut of
  Left e -> Left $ "Could not decode transaction output: " <> showt e
  Right output -> pure output

checkTxOutBtcLog :: (MonadIO m, PlatformNatives) => BtcAddress -> TxOut -> m Bool
checkTxOutBtcLog addr txout = case checkTxOutBtc addr txout of
  Left err -> do
    logWrite err
    pure False
  Right res -> pure res

-- | Checks given TxOut wheather it contains given address.
checkTxOutBtc :: BtcAddress -> TxOut -> Either Text Bool
checkTxOutBtc addr txOut = do
  decodedOutput <- decodeBtcOutHelper txOut
  case decodedOutput of
    HS.PayPKHash h | HA.PubKeyAddress pkh <- addr -> if h == pkh then pure True else pure False
    HS.PayScriptHash h | HA.ScriptAddress sh <- addr -> if h == sh then pure True else pure False
    HS.PayWitnessPKHash h | HA.WitnessPubKeyAddress wpkh <- addr -> if h == wpkh then pure True else pure False
    HS.PayWitnessScriptHash h | HA.WitnessScriptAddress wsh <- addr -> if h == wsh then pure True else pure False
    _ -> pure False

countConfirmations :: BlockHeight -> Maybe BlockHeight -> Word64
countConfirmations _ Nothing = 0
countConfirmations currentHeight (Just confirmationHeight) = currentHeight - confirmationHeight + 1

-- | Filter txs for ones, relevant to an address
filterTxsForAddressBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> [BtcTxRaw] -> m [BtcTxRaw]
filterTxsForAddressBtc addr txs = fmap catMaybes $ for txs $ \tx -> do
  b <- checkAddrTxBtc tx addr
  pure $ if b then Just tx else Nothing

-- | Gets a list of groups of conflicting txs. Txs in the same list have at least one common input.
getConflictingTxs :: [(Bool, [BtcTxId])] -> [EgvTx] -> [[BtcTxId]]
getConflictingTxs possiblyReplacedTxs txs = L.zipWith removePossiblyReplacedTxs possiblyReplacedTxs (getConflicts <$> btcTxs)
  where
    btcTxs = catMaybes $ fmap getBtcTx . toTxBtc <$> txs

    removePossiblyReplacedTxs :: (Bool, [BtcTxId]) -> [BtcTxId] -> [BtcTxId]
    removePossiblyReplacedTxs (_, prTxs) cTxs = L.filter (`L.notElem` prTxs) cTxs

    getConflicts :: BtcTxRaw -> [BtcTxId]
    getConflicts tx = HK.txHash <$> (L.filter (haveCommonInputs tx) (L.delete tx btcTxs))

isDirectChildTxOf :: BtcTxRaw -> BtcTxRaw -> Bool
isDirectChildTxOf childTx parentTx = parentTxId `L.elem` childTxInputsTxIds
  where
    parentTxId = txHash parentTx
    childTxInputsTxIds = (HK.outPointHash . HK.prevOutput) <$> HK.txIn childTx

-- | Returns the list of child txs found in transaction storage.
--           parentTx
--          /        \
--     childTx1    childTx3   => [childTx1, childTx2, childTx3]
--       /
--  childTx2
getChildTxs :: (HasTxStorage m, PlatformNatives) => BtcTxRaw -> m [BtcTxRaw]
getChildTxs tx = do
  txStore <- askTxStorage
  case L.filter (`isDirectChildTxOf` tx) (getBtcTx . fromJust . toTxBtc <$> M.elems txStore) of
    [] -> pure []
    childTxs -> do
      grandChildTxs <- L.concat <$> traverse getChildTxs childTxs
      pure $ childTxs ++ grandChildTxs

-- | Gets a list of ids of replaced transactions for every transaction in provided list.
getReplacedTxs :: M.Map BtcTxId (S.Set BtcTxId) -> [EgvTx] -> [[BtcTxId]]
getReplacedTxs replacedTxs txs = getReplaced replacedTxs <$> txs
  where
    getReplaced :: M.Map BtcTxId (S.Set BtcTxId) -> EgvTx -> [BtcTxId]
    getReplaced rTxs tx = case M.lookup (fromJust . toBtcTxHash . egvTxId $ tx) rTxs of
      Nothing -> []
      Just txSet -> S.toList txSet

-- | Gets a list of ids of possibly replaced transactions for every transaction in provided list.
getPossiblyReplacedTxs :: M.Map BtcTxId (S.Set BtcTxId) -> [EgvTx] -> [(Bool, [BtcTxId])]
getPossiblyReplacedTxs possiblyReplacedTxs txs = getPossiblyReplaced possiblyReplacedTxs <$> txs
  where
    getPossiblyReplaced :: M.Map BtcTxId (S.Set BtcTxId) -> EgvTx -> (Bool, [BtcTxId])
    getPossiblyReplaced rTxs tx = M.foldrWithKey' (helper $ fromJust . toBtcTxHash . egvTxId $ tx) (False, []) rTxs

    helper :: BtcTxId -> BtcTxId -> S.Set BtcTxId -> (Bool, [BtcTxId]) -> (Bool, [BtcTxId])
    helper txId possiblyReplacingTxId possiblyReplacedTxIds acc
      | txId == possiblyReplacingTxId = (True, S.toList possiblyReplacedTxIds)
      | S.member txId possiblyReplacedTxIds = (False, possiblyReplacingTxId : (S.toList $ S.delete txId possiblyReplacedTxIds))
      | otherwise = acc

getOutputByOutPoint :: (HasTxStorage m, PlatformNatives) => HK.OutPoint -> m (Maybe HK.TxOut)
getOutputByOutPoint HK.OutPoint{..} = do
  mtx <- getTxById $ hkTxHashToEgv outPointHash
  case mtx of
    Nothing -> pure Nothing
    Just (TxErg _) -> pure Nothing -- TODO: impl for Ergo
    Just (TxBtc BtcTx{..}) -> pure $ Just $ (HK.txOut getBtcTx) !! (fromIntegral outPointIndex)

getOutputsByOutPoints :: (HasTxStorage m, PlatformNatives) => [HK.OutPoint] -> m [Maybe HK.TxOut]
getOutputsByOutPoints outPoints = traverse getOutputByOutPoint outPoints

-- | Gets spent output (they are inputs for a tx) for a given address from a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getSpentOutputsBtc :: (HasTxStorage m, PlatformNatives) => Bool -> BtcAddress -> BtcTxRaw -> m ([(OutPoint, Bool)])
getSpentOutputsBtc c addr HK.Tx{..} = fmap catMaybes $ for txIn $ \ti -> do
  b <- checkTxInBtc addr ti
  pure $ if b then Just (prevOutput ti, c) else Nothing

-- | Calculates tx fee. If any of parent txs were are found in the wallet storage then it returns Nothing.
getTxFee :: (HasTxStorage m, PlatformNatives) => BtcTxRaw -> m (Maybe Word64)
getTxFee tx = do
  let inputs = HK.txIn tx
      outputs = HK.txOut tx
      outputsAmount = L.sum $ HK.outValue <$> outputs
  prevOutputs <- getOutputsByOutPoints $ HK.prevOutput <$> inputs
  let inputsAmount = L.sum $ HK.outValue <$> catMaybes prevOutputs
      result = if L.any isNothing prevOutputs then Nothing else Just $ inputsAmount - outputsAmount
  pure result

-- | Calculates amount of coins in tx outputs.
getTxOutputsAmount :: BtcTxRaw -> Word64
getTxOutputsAmount tx = L.sum $ HK.outValue <$> outputs
  where outputs = HK.txOut tx

-- | Gets unspent output for a given address from a transaction
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUnspentOutputs :: (MonadIO m, PlatformNatives)
  => Maybe BlockHeight -> ScanKeyBox -> BtcTxRaw -> m [(OutPoint, BtcUtxoMeta)]
getUnspentOutputs c ScanKeyBox{..} tx = fmap catMaybes $ for (L.zip [0..] $ txOut tx) $ \(i,o) -> do
  b <- checkTxOutBtcLog addr o
  let escript = HS.decodeOutputBS $ scriptOutput o
  either (\e -> logWrite $ "Failed to decode scriptOutput: " <> showt o <> ". Error: " <> showt e) (const $ pure ()) escript
  pure $ case (b, escript) of
    (True, Right scr) -> Just (OutPoint th i, BtcUtxoMeta scanBox'index scanBox'purpose (outValue o) scr stat)
    _ -> Nothing
  where
    th = txHash tx
    stat = maybe (EUtxoReceiving Nothing) EUtxoSemiConfirmed c
    addr = xPubToBtcAddr $ extractXPubKeyFromEgv scanBox'key

-- | Construct UTXO update for a list of addresses based on a transaction
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUtxoUpdates :: (HasTxStorage m, PlatformNatives)
  => Maybe BlockHeight -> V.Vector ScanKeyBox -> BtcTxRaw -> m BtcUtxoUpdate
getUtxoUpdates mheight boxes tx = do
  (unsps, sps) <- fmap V.unzip $ for boxes $ \box -> do
    let addr = xPubToBtcAddr . extractXPubKeyFromEgv . scanBox'key $ box
    unsp <- getUnspentOutputs mheight box tx
    sp   <- getSpentOutputsBtc isConfirmed addr tx
    pure (unsp, sp)
  let unspentMap = M.fromList $ mconcat $ V.toList unsps
  pure (unspentMap, mconcat $ V.toList $ sps)
  where isConfirmed = maybe False (const True) mheight

-- | Construct UTXO update for an address and a batch of transactions
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUtxoUpdatesFromTxs :: (HasTxStorage m, PlatformNatives)
  => Maybe BlockHeight -> ScanKeyBox -> [BtcTxRaw] -> m BtcUtxoUpdate
getUtxoUpdatesFromTxs mheight box txs = do
  (unsps, sps) <- fmap unzip $ for txs $ \tx -> do
    unsp <- getUnspentOutputs mheight box tx
    sp   <- getSpentOutputsBtc isConfirmed addr tx
    pure (unsp, sp)
  let unspentMap = M.fromList $ mconcat unsps
  pure (unspentMap, mconcat sps)
  where
    isConfirmed = maybe False (const True) mheight
    addr = xPubToBtcAddr . extractXPubKeyFromEgv . scanBox'key $ box

haveCommonInputs :: BtcTxRaw -> BtcTxRaw -> Bool
haveCommonInputs tx1 tx2 = (not . L.null) $ (prevOutput <$> txIn tx1) `L.intersect` (prevOutput <$> txIn tx2)

-- | Returns Just True if tx1 has higher fee than tx2, Just False if tx2 has higher fee than tx1.
-- If it was not possible to calculate the fee for any of the transactions
-- and transactions have different sets of inputs, returns Nothing.
haveHigherFee :: (HasTxStorage m, PlatformNatives) => BtcTxRaw -> BtcTxRaw -> m (Maybe Bool)
haveHigherFee tx1 tx2
  | haveSameInputs tx1 tx2 = pure $ Just $ getTxOutputsAmount tx1 < getTxOutputsAmount tx2
  | otherwise = do
      mFee1 <- getTxFee tx1
      mFee2 <- getTxFee tx2
      case (mFee1, mFee2) of
        (Just fee1, Just fee2) -> pure $ Just $ fee1 > fee2
        (_, _) -> pure Nothing

haveSameInputs :: BtcTxRaw -> BtcTxRaw -> Bool
haveSameInputs tx1 tx2 = L.sort (prevOutput <$> txIn tx1) == L.sort (prevOutput <$> txIn tx2)

-- | Check given TxIn wheather it spends OutPoint.
inputSpendsOutPoint :: OutPoint -> TxIn -> Bool
inputSpendsOutPoint outPoint txIn = prevOutput txIn == outPoint

markedReplaceable :: BtcTxRaw -> Bool
markedReplaceable tx = L.any (\input -> txInSequence input < (maxBound :: Word32) - 1) (txIn tx)

-- | Returns Just True if it is known for sure that tx1 replaces tx2 by fee.
-- Returns Just False if it is known for sure that tx1 not replaces tx2 by fee.
-- Returns Nothing if tx1 and tx2 have common inputs but we don't know which of txs have higher fee.
replacesByFee :: (HasTxStorage m, PlatformNatives) => BtcTxRaw -> BtcTxRaw -> m (Maybe Bool)
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

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of recipient addresses and amounts as outputs.
buildAddrTxRbf :: Network -> [OutPoint] -> [(Text, Word64)] -> Either String Tx
buildAddrTxRbf net xs ys = buildTxRbf xs =<< mapM f ys
  where
    f (s, v) =
        maybe (Left ("buildAddrTxRbf: Invalid address " <> T.unpack s)) Right $ do
            a <- HA.stringToAddr net s
            let o = HA.addressToOutput a
            return (o, v)

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTxRbf :: [OutPoint] -> [(HS.ScriptOutput, Word64)] -> Either String Tx
buildTxRbf xs ys =
    mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint B.empty (maxBound - 2)
    fo (o, v)
        | v <= 2100000000000000 = return $ TxOut v $ HS.encodeOutputBS o
        | otherwise =
            Left $ "buildTxRbf: Invalid amount " ++ show v
