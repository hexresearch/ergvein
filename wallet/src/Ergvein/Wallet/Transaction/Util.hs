{-# OPTIONS_GHC -Wall #-}

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
  , buildAddrTx
  , weightUnitsToVBytes
  , calcTxVsize
  , getUtxoByInput
  , getUtxoByInputs
  , decodeBtcOutHelper
  , unpackOut
  , signTxWithWallet
  , getInternalKeyboxByOutput
  -- Ergo functions
  ) where

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Data.Ratio
import Data.Serialize
import Data.Text (Text)
import Data.Traversable (for)
import Data.Word
import Network.Haskoin.Network (putVarInt)
import Network.Haskoin.Transaction (Tx(..), TxIn(..), TxOut(..), OutPoint(..), txHash)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network (Network)
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Types.Utxo.Btc
import Ergvein.Util
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Platform

import qualified Data.ByteString                    as B
import qualified Data.List                          as L
import qualified Data.Map.Strict                    as M
import qualified Data.Set                           as S
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Network.Haskoin.Address            as HA
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT

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
  checkTxInputsResults <- traverse (checkTxInBtc addr) (HT.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOutBtcLog addr) (HT.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = L.foldr (||) False

checkAddrTxErg :: (HasTxStorage m, PlatformNatives) => ErgTxRaw -> ErgAddress -> m Bool
checkAddrTxErg = undefined

-- | Checks given tx if there are some inputs containing given address.
checkAddrTxInBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> BtcTxRaw -> m Bool
checkAddrTxInBtc addr tx = do
  checkTxInputsResults <- traverse (checkTxInBtc addr) (HT.txIn tx)
  pure $ or checkTxInputsResults

-- | Checks given tx if there are some outputs containing given address.
checkAddrTxOutBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> BtcTxRaw -> m Bool
checkAddrTxOutBtc addr tx = do
  checkTxOutputsResults <- traverse (checkTxOutBtcLog addr) (HT.txOut tx)
  pure $ or checkTxOutputsResults

checkOutIsOursBtc :: (MonadIO m, PlatformNatives) => [BtcAddress] -> HT.TxOut -> m Bool
checkOutIsOursBtc addrs out = do
  results <- traverse (flip checkTxOutBtcLog out) addrs
  pure $ L.any (== True) results

checkOutSpent :: [[HT.TxIn]] -> HT.OutPoint -> Bool
checkOutSpent inputs out = let results = (fmap . fmap) (inputSpendsOutPoint out) inputs in
  or $ fmap or results

-- | Checks given TxIn whether it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxInBtc :: (HasTxStorage m, PlatformNatives) => BtcAddress -> TxIn -> m Bool
checkTxInBtc addr txIn = do
  let spentOutput = HT.prevOutput txIn
      spentTxHash = HT.outPointHash spentOutput
      spentOutputIndex = HT.outPointIndex spentOutput
  mtx <- getTxById $ hkTxHashToEgv spentTxHash
  case mtx of
    Nothing -> pure False
    Just (TxErg _) -> pure False -- TODO: impl for Ergo
    Just (TxBtc BtcTx{..}) -> checkTxOutBtcLog addr $ HT.txOut getBtcTx !! fromIntegral spentOutputIndex

decodeBtcOutHelper :: TxOut -> Either Text HS.ScriptOutput
decodeBtcOutHelper txOut = case HS.decodeOutputBS $ HT.scriptOutput txOut of
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
    getConflicts tx = HT.txHash <$> (L.filter (haveCommonInputs tx) (L.delete tx btcTxs))

isDirectChildTxOf :: BtcTxRaw -> BtcTxRaw -> Bool
isDirectChildTxOf childTx parentTx = parentTxId `L.elem` childTxInputsTxIds
  where
    parentTxId = txHash parentTx
    childTxInputsTxIds = (HT.outPointHash . HT.prevOutput) <$> HT.txIn childTx

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

getOutputByOutPoint :: (HasTxStorage m, PlatformNatives) => HT.OutPoint -> m (Maybe HT.TxOut)
getOutputByOutPoint HT.OutPoint{..} = do
  mtx <- getTxById $ hkTxHashToEgv outPointHash
  case mtx of
    Nothing -> pure Nothing
    Just (TxErg _) -> pure Nothing -- TODO: impl for Ergo
    Just (TxBtc BtcTx{..}) -> pure $ Just $ (HT.txOut getBtcTx) !! (fromIntegral outPointIndex)

getOutputsByOutPoints :: (HasTxStorage m, PlatformNatives) => [HT.OutPoint] -> m [Maybe HT.TxOut]
getOutputsByOutPoints outPoints = traverse getOutputByOutPoint outPoints

-- | Gets spent output (they are inputs for a tx) for a given address from a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getSpentOutputsBtc :: (HasTxStorage m, PlatformNatives) => Bool -> BtcAddress -> BtcTxRaw -> m ([(OutPoint, Bool)])
getSpentOutputsBtc c addr HT.Tx{..} = fmap catMaybes $ for txIn $ \ti -> do
  b <- checkTxInBtc addr ti
  pure $ if b then Just (prevOutput ti, c) else Nothing

-- | Calculates tx fee. If any of the parent txs was not found in the wallet storage then it returns Nothing.
getTxFee :: (HasTxStorage m, PlatformNatives) => BtcTxRaw -> m (Maybe Word64)
getTxFee tx = do
  let inputs = HT.txIn tx
      outputs = HT.txOut tx
      outputsAmount = L.sum $ HT.outValue <$> outputs
  prevOutputs <- getOutputsByOutPoints $ HT.prevOutput <$> inputs
  let inputsAmount = L.sum $ HT.outValue <$> catMaybes prevOutputs
      result = if L.any isNothing prevOutputs then Nothing else Just $ inputsAmount - outputsAmount
  pure result

-- | Calculates amount of coins in tx outputs.
getTxOutputsAmount :: BtcTxRaw -> Word64
getTxOutputsAmount tx = L.sum $ HT.outValue <$> outputs
  where outputs = HT.txOut tx

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
buildAddrTx :: Network -> RbfEnabled -> [OutPoint] -> [(Text, Word64)] -> Either String Tx
buildAddrTx net rbfEnabled xs ys = buildTx rbfEnabled xs =<< mapM f ys
  where
    f (s, v) =
      maybe (Left ("buildAddrTx: Invalid address " <> T.unpack s)) Right $ do
        a <- HA.stringToAddr net s
        let o = HA.addressToOutput a
        return (o, v)

-- | Build a transaction by providing a list of outpoints as inputs
-- and a list of 'ScriptOutput' and amounts as outputs.
buildTx :: RbfEnabled -> [OutPoint] -> [(HS.ScriptOutput, Word64)] -> Either String Tx
buildTx rbfEnabled xs ys =
  mapM fo ys >>= \os -> return $ Tx 1 (map fi xs) os [] 0
  where
    fi outPoint = TxIn outPoint B.empty rbf
    rbf = if rbfEnabled then (maxBound - 2) else maxBound
    fo (o, v)
      | v <= 2100000000000000 = return $ TxOut v $ HS.encodeOutputBS o
      | otherwise = Left $ "buildTx: Invalid amount " ++ show v

weightUnitsToVBytes :: Word64 -> Int
weightUnitsToVBytes wu = ceiling $ wu % 4

calcTxVsize :: Tx -> Int
calcTxVsize tx
  | null (txWitness tx) = calcLegacyTxVsize tx
  | otherwise = calcWitnessTxVsize tx

calcLegacyTxVsize :: Tx -> Int
calcLegacyTxVsize = B.length . encode

calcWitnessTxVsize :: Tx -> Int
calcWitnessTxVsize tx = weightUnitsToVBytes $ overhead + inOut + witnessData
  where
    overhead = 34 -- version 16 WU + marker 1 WU + flag 1 WU + locktime 16 WU
    inOut = (* 4) $ fromIntegral $ B.length $ runPut $ putInOut tx
    witnessData = fromIntegral $ B.length $ runPut $ putWitnessData $ txWitness tx

-- From haskoin-core
putInOut :: Tx -> Put
putInOut tx = do
  putVarInt $ length (txIn tx)
  forM_ (txIn tx) put
  putVarInt $ length (txOut tx)
  forM_ (txOut tx) put

-- | Witness data serializer.
putWitnessData :: HT.WitnessData -> Put
putWitnessData = mapM_ putWitnessStack
  where
    putWitnessStack ws = do
      putVarInt $ length ws
      mapM_ putWitnessStackItem ws
    putWitnessStackItem bs = do
      putVarInt $ B.length bs
      putByteString bs

getUtxoByInputs :: (HasPubStorage m, PlatformNatives) => [TxIn] -> m (Either Text [UtxoPoint])
getUtxoByInputs inputs = do
  eResults <- traverse getUtxoByInput inputs
  case allRight eResults of
    Nothing -> pure $ Left "getUtxoByInputs: couldn't get some utxo by given inputs"
    Just results -> pure $ Right results

getUtxoByInput :: (HasPubStorage m, PlatformNatives) => TxIn -> m (Either Text UtxoPoint)
getUtxoByInput input = do
  ps <- askPubStorage
  let btcPs = ps ^. btcPubStorage
      txStore = btcPs ^. currencyPubStorage'transactions
      outPoint = prevOutput input
  mOutput <- liftIO $ flip runReaderT txStore $ getOutputByOutPoint outPoint
  case mOutput of
    Nothing -> pure $ Left "getUtxoByInput: couldn't get output"
    Just output -> do
      case HS.decodeOutputBS $ scriptOutput output of
        Left e -> pure $ Left $ "getUtxoByInput: " <> T.pack e
        Right script -> do
          mKeyBox <- getKeyBoxByInput input
          case mKeyBox of
            Nothing -> pure $ Left $ "getUtxoByInput: couldn't get keybox"
            Just keyBox -> do
              let meta = BtcUtxoMeta {
                      btcUtxo'index   = scanBox'index keyBox
                    , btcUtxo'purpose = scanBox'purpose keyBox
                    , btcUtxo'amount  = outValue output
                    , btcUtxo'script  = script
                    , btcUtxo'status  = EUtxoSending Nothing
                  }
              pure $ Right $ UtxoPoint outPoint meta

getKeyBoxByInput :: (MonadIO m, HasPubStorage m, PlatformNatives) => TxIn -> m (Maybe ScanKeyBox)
getKeyBoxByInput input = do
  ps <- askPubStorage
  let btcPs = ps ^. btcPubStorage
      txStore = btcPs ^. currencyPubStorage'transactions
      pubKeyStore = btcPs ^. currencyPubStorage'pubKeystore
      externalKeys = getExternalPublicKeys pubKeyStore
      internalKeys = getInternalPublicKeys pubKeyStore
  matchedKeys <- liftIO $ flip runReaderT txStore $ V.filterM filterHelper $ externalKeys <> internalKeys
  pure $ matchedKeys V.!? 0
  where
    filterHelper :: (HasTxStorage m, PlatformNatives) => ScanKeyBox -> m Bool
    filterHelper keyBox =
      let addr = xPubToBtcAddr $ extractXPubKeyFromEgv $ scanBox'key keyBox
      in checkTxInBtc addr input

getInternalKeyboxByOutput :: PubKeystore -> TxOut -> Maybe EgvPubKeyBox
getInternalKeyboxByOutput PubKeystore{..} txOut = matchedKeys V.!? 0
  where
    mDecodedOut = eitherToMaybe $ decodeBtcOutHelper txOut
    mAddr = HA.outputAddress =<< mDecodedOut
    matchedKeys = V.filter filterHelper pubKeystore'internal

    filterHelper :: EgvPubKeyBox -> Bool
    filterHelper keyBox =
      let addr = xPubToBtcAddr $ extractXPubKeyFromEgv $ pubKeyBox'key keyBox
      in Just addr == mAddr

unpackOut :: TxOut -> Maybe (Text, Word64)
unpackOut txOut = (, amount) <$> mAddr
   where
    mDecodedOut = eitherToMaybe $ decodeBtcOutHelper txOut
    mAddr = btcAddrToString <$> (HA.outputAddress =<< mDecodedOut)
    amount = HT.outValue txOut

-- | Sign function which has access to the private storage
-- TODO: generate missing private keys
signTxWithWallet :: (MonadIO m, PlatformNatives) => Tx -> [UtxoPoint] -> PrvStorage -> m (Maybe (Either String Tx))
signTxWithWallet tx pick prv = do
  let PrvKeystore _ ext int = prv ^. prvStorage'currencyPrvStorages
        . at BTC . non (error "btcSendConfirmationWidget: not exsisting store!")
        . currencyPrvStorage'prvKeystore
  mvals <- fmap (fmap unzip . sequence) $ flip traverse pick $ \(UtxoPoint opoint BtcUtxoMeta{..}) -> do
    let sig = HT.SigInput btcUtxo'script btcUtxo'amount opoint HS.sigHashAll Nothing
    let errMsg = "Failed to get a corresponding secret key: " <> showt btcUtxo'purpose <> " #" <> showt btcUtxo'index
    let msec = case btcUtxo'purpose of
          Internal -> fmap (xPrvKey . unEgvXPrvKey) $ (V.!?) int btcUtxo'index
          External -> fmap (xPrvKey . unEgvXPrvKey) $ (V.!?) ext btcUtxo'index
    maybe (logWrite errMsg >> pure Nothing) (pure . Just . (sig,)) msec
  pure $ (uncurry $ HT.signTx btcNetwork tx) <$> mvals
