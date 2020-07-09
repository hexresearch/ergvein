module Ergvein.Types.Utxo
  (
    EgvUtxoStatus(..)
  , BtcUtxoSet
  , BtcUtxoUpdate
  , UtxoMeta(..)
  , isUtxoConfirmed
  , updateBtcUtxoSetPure
  , reconfirmBtxUtxoSetPure
  ) where

import Data.Aeson
import Data.List (foldl')
import Data.Word
import Network.Haskoin.Script
import Network.Haskoin.Transaction

import Ergvein.Aeson
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data EgvUtxoStatus
  = EUtxoConfirmed
  | EUtxoSemiConfirmed !BlockHeight
  | EUtxoSending !(Maybe BlockHeight)
  | EUtxoReceiving !(Maybe BlockHeight)

  deriving (Eq, Show, Read)
$(deriveJSON defaultOptions ''EgvUtxoStatus)

isUtxoConfirmed :: EgvUtxoStatus -> Bool
isUtxoConfirmed v = case v of
  EUtxoConfirmed -> True
  _ -> False

data UtxoMeta = UtxoMeta {
  utxoMeta'index   :: !Int
, utxoMeta'purpose :: !KeyPurpose
, utxoMeta'amount  :: !Word64
, utxoMeta'script  :: !ScriptOutput
, utxoMeta'status  :: !EgvUtxoStatus
} deriving (Eq, Show, Read)

$(deriveJSON aesonOptionsStripToApostroph ''UtxoMeta)

type BtcUtxoSet = M.Map OutPoint UtxoMeta

-- | fst -- wallet's unspent outputs
-- snd -- wallet's spent outputs
-- snd's bool: True - confirmed, must be deleted from UTXO set, False - set status to EUtxoSending
type BtcUtxoUpdate = (BtcUtxoSet, [(OutPoint, Bool)])

instance FromJSONKey OutPoint
instance ToJSONKey OutPoint

updateBtcUtxoSetPure :: BtcUtxoUpdate -> BtcUtxoSet -> BtcUtxoSet
updateBtcUtxoSetPure (outs, ins) s = foo (M.union outs s) ins $ \m (op, b) ->
  M.update (\meta -> if b then Nothing else Just meta {utxoMeta'status = EUtxoSending Nothing}) op m
  where foo b ta f = foldl' f b ta

confirmationGap :: Word64
confirmationGap = 3

staleGap :: Word64
staleGap = 15     -- ~1 blk / 10 min. 14 days * 24 h * 6 blocks/h.

reconfirmBtxUtxoSetPure :: BlockHeight -> BtcUtxoSet -> BtcUtxoSet
reconfirmBtxUtxoSetPure bh bs = flip M.mapMaybe bs $ \meta -> case utxoMeta'status meta of
  EUtxoConfirmed -> Just $ meta
  EUtxoSemiConfirmed bh0 -> Just $ if bh - bh0 >= confirmationGap - 1 then meta {utxoMeta'status = EUtxoConfirmed} else meta
  EUtxoSending mh -> case mh of
    Nothing -> Just $ meta {utxoMeta'status = EUtxoSending $ Just bh}
    Just bh0 -> if bh - bh0 >= staleGap - 1 then Nothing else Just meta
  EUtxoReceiving mh -> case mh of
    Nothing -> Just $ meta {utxoMeta'status = EUtxoSending $ Just bh}
    Just bh0 -> if bh - bh0 >= staleGap - 1 then Nothing else Just meta
  where
    keys = M.keys bs
    foo b ta f = foldl' f b ta
