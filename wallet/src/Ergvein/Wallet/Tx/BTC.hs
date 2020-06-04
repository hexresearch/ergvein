module Ergvein.Wallet.Tx.BTC (
    checkAddrTx
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Dependent.Map (DMap, DSum((:=>)))
import Data.List
import Data.Map.Strict (Map)
import Ergvein.Aeson
import Ergvein.Crypto.Keys
import Ergvein.Text
import Ergvein.Types.Address
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Blocks.BTC
import Ergvein.Wallet.Blocks.Storage
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Log.Event
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Storage.Constants
import Ergvein.Wallet.Storage.Keys (derivePubKey, egvXPubKeyToEgvAddress)
import Ergvein.Wallet.Storage.Util (addXPubKeyToKeystore)
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map                 as DM
import qualified Data.IntMap.Strict                 as MI
import qualified Data.Map.Strict                    as M
import qualified Data.Text                          as T
import qualified Ergvein.Wallet.Filters.Scan        as Filters
import qualified Network.Haskoin.Address            as HA
import qualified Network.Haskoin.Block              as HB
import qualified Network.Haskoin.Script             as HS
import qualified Network.Haskoin.Transaction        as HT

-- | Checks given tx if there are some inputs or outputs containing given address.
checkAddrTx :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HT.Tx -> m Bool
checkAddrTx addr tx = do
  checkTxInputsResults <- traverse (checkTxIn addr) (HT.txIn tx)
  checkTxOutputsResults <- traverse (checkTxOut addr) (HT.txOut tx)
  pure $ concatResults checkTxInputsResults || concatResults checkTxOutputsResults
  where concatResults = foldr (||) False

-- | Checks given TxIn wheather it contains given address.
-- Native SegWit addresses are not presented in TxIns scriptSig.
checkTxIn :: (MonadIO m, HasBlocksStorage m, PlatformNatives) => EgvAddress -> HT.TxIn -> m Bool
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
checkTxOut :: (MonadIO m, PlatformNatives) => EgvAddress -> HT.TxOut -> m Bool
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
