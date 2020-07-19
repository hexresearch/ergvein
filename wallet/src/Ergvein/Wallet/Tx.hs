module Ergvein.Wallet.Tx
  (
    checkAddrTx
  , filterTxsForAddress
  , getSpentOutputs
  , getUnspentOutputs
  , getUtxoUpdates
  , getUtxoUpdatesFromTxs
  ) where

import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Word
import Network.Haskoin.Transaction (Tx(..), TxIn(..), TxOut(..), OutPoint(..), txHash)

import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.Keys
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node.BTC.Blocks
import Ergvein.Wallet.Storage.Keys

import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as T
import qualified Data.Vector                        as V
import qualified Network.Haskoin.Address            as HA
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT


-- | Filter txs for ones, relevant to an address
filterTxsForAddress :: (HasTxStorage m, PlatformNatives) => EgvAddress -> [Tx] -> m [Tx]
filterTxsForAddress addr txs = fmap catMaybes $ flip traverse txs $ \tx -> do
  b <- checkAddrTx addr tx
  pure $ if b then Just tx else Nothing
  
-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (HasTxStorage m, PlatformNatives) => EgvAddress -> Tx -> m Bool
checkAddrTx addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HT.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOut addr) (HT.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = foldr (||) False

-- | Gets spent output (they are inputs for a tx) for a given address from a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getSpentOutputs :: (HasTxStorage m, PlatformNatives) => Bool -> EgvAddress -> Tx -> m ([(OutPoint, Bool)])
getSpentOutputs c addr Tx{..} = fmap catMaybes $ flip traverse txIn $ \ti -> do
  b <- checkTxIn addr ti
  pure $ if b then Just (prevOutput ti, c) else Nothing

-- | Gets unspent output for a given address from a transaction
-- Maybe BlockHeight: Nothing -- unconfirmed Tx. Just n -> confirmed at height n
getUnspentOutputs :: (MonadIO m, PlatformNatives)
  => Maybe BlockHeight -> ScanKeyBox -> Tx -> m [(OutPoint, UtxoMeta)]
getUnspentOutputs c ScanKeyBox{..} tx = fmap catMaybes $ flip traverse (zip [0..] $ txOut tx) $ \(i,o) -> do
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

-- | Checks given TxIn wheather it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxIn :: (HasTxStorage m, PlatformNatives) => EgvAddress -> TxIn -> m Bool
checkTxIn addr txIn = do
  let spentOutput = HT.prevOutput txIn
      spentTxHash = HT.outPointHash spentOutput
      spentOutputIndex = HT.outPointIndex spentOutput
  mtx <- getTxById $ HT.txHashToHex spentTxHash
  case mtx of
    Nothing -> pure False
    Just ErgTx{} -> pure False -- TODO: impl for Ergo
    Just BtcTx{..} -> checkTxOut addr $ (HT.txOut getBtcTx) !! (fromIntegral spentOutputIndex)

-- | Checks given TxOut wheather it contains given address.
-- TODO: Pattern match(es) are non-exhaustive:
--       Patterns not matched:
--           (BtcAddress (HA.PubKeyAddress _)) _
--           (BtcAddress (HA.ScriptAddress _)) _
--           (ErgAddress _) _
checkTxOut :: (MonadIO m, PlatformNatives) => EgvAddress -> TxOut -> m Bool
checkTxOut (BtcAddress (HA.WitnessPubKeyAddress pkh)) txO = case HS.decodeOutputBS $ HT.scriptOutput txO of
  Left e -> do
    logWrite $ "Could not decode transaction output " <> (showt e)
    pure False
  Right output -> case output of
    HS.PayWitnessPKHash h -> if h == pkh then pure True else pure False
    _ -> pure False
checkTxOut (BtcAddress (HA.WitnessScriptAddress sh)) txO = case HS.decodeOutputBS $ HT.scriptOutput txO of
  Left e -> do
    logWrite $ "Could not decode transaction output " <> (showt e)
    pure False
  Right output -> case output of
    HS.PayWitnessScriptHash h -> if h == sh then pure True else pure False
    _ -> pure False
