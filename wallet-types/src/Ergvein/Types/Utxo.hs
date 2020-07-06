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
import Network.Haskoin.Transaction

import Ergvein.Aeson
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Transaction

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data EgvUtxoStatus = EUtxoConfirmed | EUtxoSemiConfirmed BlockHeight | EUtxoSending | EUtxoReceiving
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
  M.update (\meta -> if b then Nothing else Just meta {utxoMeta'status = EUtxoSending}) op m
  where foo b ta f = foldl' f b ta

confirmationGap :: Word64
confirmationGap = 3

reconfirmBtxUtxoSetPure :: BlockHeight -> BtcUtxoSet -> BtcUtxoSet
reconfirmBtxUtxoSetPure bh = fmap $ \meta -> case utxoMeta'status meta of
  EUtxoSemiConfirmed bh0 -> if bh - bh0 >= confirmationGap - 1 then meta {utxoMeta'status = EUtxoConfirmed} else meta
  _ -> meta
