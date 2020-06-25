module Ergvein.Wallet.Tx
  (
    checkAddrTx
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
import Ergvein.Types.Utxo
import Ergvein.Wallet.Blocks.BTC
import Ergvein.Wallet.Blocks.Storage
import Ergvein.Wallet.Native

import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as T
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT
import qualified Network.Haskoin.Address            as HA

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> Tx -> m Bool
checkAddrTx addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HT.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOut addr) (HT.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = foldr (||) False

-- | Gets spent output (they are inputs for a tx) for a given address from a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getSpentOutputs :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => Bool -> EgvAddress -> Tx -> m ([(OutPoint, Bool)])
getSpentOutputs c addr Tx{..} = fmap catMaybes $ flip traverse txIn $ \ti -> do
  b <- checkTxIn addr ti
  pure $ if b then Just (prevOutput ti, c) else Nothing

-- | Gets unspent output for a given address from a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getUnspentOutputs :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => Bool -> EgvAddress -> Tx -> m [(OutPoint, (Word64, EgvUtxoStatus))]
getUnspentOutputs c addr tx = fmap catMaybes $ flip traverse (zip [0..] $ txOut tx) $ \(i,o) -> do
  b <- checkTxOut addr o
  pure $ if b then Just (OutPoint th i, (outValue o, stat)) else Nothing
  where
    th = txHash tx
    stat = if c then EUtxoConfirmed else EUtxoReceiving

-- | Construct UTXO update for a list of addresses based on a transaction
-- Bool specifies if the Tx was confirmed (True) or not
getUtxoUpdates :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => Bool -> [EgvAddress] -> Tx -> m BtcUtxoUpdate
getUtxoUpdates isConfirmed addrs tx = do
  (unsps, sps) <- fmap unzip $ flip traverse addrs $ \addr -> do
    unsp <- getUnspentOutputs isConfirmed addr tx
    sp   <- getSpentOutputs isConfirmed addr tx
    pure (unsp, sp)
  let unspentMap = M.fromList $ mconcat unsps
  pure (unspentMap, mconcat sps)

-- | Construct UTXO update for an address and a batch of transactions
-- Bool specifies if the batch was confirmed (True) or not
getUtxoUpdatesFromTxs :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => Bool -> EgvAddress -> [Tx] -> m BtcUtxoUpdate
getUtxoUpdatesFromTxs isConfirmed addr txs = do
  (unsps, sps) <- fmap unzip $ flip traverse txs $ \tx -> do
    unsp <- getUnspentOutputs isConfirmed addr tx
    sp   <- getSpentOutputs isConfirmed addr tx
    pure (unsp, sp)
  let unspentMap = M.fromList $ mconcat unsps
  pure (unspentMap, mconcat sps)

-- | Checks given TxIn wheather it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxIn :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> TxIn -> m Bool
checkTxIn addr txIn = do
  let spentOutput = HT.prevOutput txIn
      spentTxHash = HT.outPointHash spentOutput
      spentOutputIndex = HT.outPointIndex spentOutput
  mBlockHash <- getBtcBlockHashByTxHash spentTxHash
  case mBlockHash of
    Nothing -> pure False
    Just blockHash -> do
      mBlock <- getBtcBlock blockHash
      case mBlock of
        Nothing -> fail $ "Could not get block from storage by block hash " <> (T.unpack $ HB.blockHashToHex blockHash)
        Just block -> do
          let mSpentTx = find (\tx -> HT.txHash tx == spentTxHash) (HB.blockTxns block)
          case mSpentTx of
            Nothing -> pure False
            Just spentTx -> do
              checkResult <- checkTxOut addr $ (HT.txOut spentTx) !! (fromIntegral spentOutputIndex)
              pure checkResult

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
