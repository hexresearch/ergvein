module Ergvein.Types.Utxo.Ergo
  ( ErgoUtxoSet
  , ErgoUtxoUpdate
  , ErgoUtxoMeta(..)
  , isUtxoConfirmed
  , updateErgoUtxoSetPure
  , reconfirmErgoUtxoSetPure
  , confirmationGap
  ) where

import Data.List (foldl')
import Data.SafeCopy
import Data.Serialize
import Data.Word
import Network.Haskoin.Script
import Network.Haskoin.Transaction

import Ergvein.Aeson
import Ergvein.Types.Keys
import Ergvein.Types.Transaction
import Ergvein.Types.Utxo.Status

import qualified Data.Map.Strict as M

data ErgoUtxoMeta = ErgoUtxoMeta {
  ergoUtxo'index   :: !Int
, ergoUtxo'purpose :: !KeyPurpose
, ergoUtxo'amount  :: !Word64
, ergoUtxo'script  :: !ScriptOutput
, ergoUtxo'status  :: !EgvUtxoStatus
} deriving (Eq, Show, Read)

$(deriveJSON aesonOptionsStripToApostroph ''ErgoUtxoMeta)

instance SafeCopy ErgoUtxoMeta where
  version = 1
  putCopy ErgoUtxoMeta{..} = contain $ do
    put ergoUtxo'index
    put ergoUtxo'purpose
    put ergoUtxo'amount
    put $ encodeOutputBS ergoUtxo'script
    safePut ergoUtxo'status
  getCopy = contain $ do
    index <- get
    purpose <- get
    amount <- get
    escript <- fmap decodeOutputBS get
    status <- safeGet
    case escript of
      Left err -> fail $ "failed to decode output script! " <> show err
      Right script -> pure $ ErgoUtxoMeta index purpose amount script status

type ErgoUtxoSet = M.Map OutPoint ErgoUtxoMeta

-- | fst -- wallet's unspent outputs
-- snd -- wallet's spent outputs
-- snd's bool: True - confirmed, must be deleted from UTXO set, False - set status to EUtxoSending
type ErgoUtxoUpdate = (ErgoUtxoSet, [(OutPoint, Bool)])

updateErgoUtxoSetPure :: ErgoUtxoUpdate -> ErgoUtxoSet -> ErgoUtxoSet
updateErgoUtxoSetPure (outs, ins) s = foo (M.union outs s) ins $ \m (op, b) ->
  M.update (\meta -> if b then Nothing else Just meta {ergoUtxo'status = EUtxoSending Nothing}) op m
  where foo b ta f = foldl' f b ta

confirmationGap :: Word64
confirmationGap = 3

staleGap :: Word64
staleGap = 10080     -- ~1 blk / 2 min. 14 days * 24 h * 6 blocks/h.

reconfirmErgoUtxoSetPure :: BlockHeight -> ErgoUtxoSet -> ErgoUtxoSet
reconfirmErgoUtxoSetPure bh bs = flip M.mapMaybe bs $ \meta -> case ergoUtxo'status meta of
  EUtxoConfirmed -> Just $ meta
  EUtxoSemiConfirmed bh0 -> Just $ if bh - bh0 >= confirmationGap - 1 then meta {ergoUtxo'status = EUtxoConfirmed} else meta
  EUtxoSending mh -> case mh of
    Nothing -> Just $ meta {ergoUtxo'status = EUtxoSending $ Just bh}
    Just bh0 -> if bh - bh0 >= staleGap - 1 then Nothing else Just meta
  EUtxoReceiving mh -> case mh of
    Nothing -> Just $ meta {ergoUtxo'status = EUtxoReceiving $ Just bh}
    Just bh0 -> if bh - bh0 >= staleGap - 1 then Nothing else Just meta
