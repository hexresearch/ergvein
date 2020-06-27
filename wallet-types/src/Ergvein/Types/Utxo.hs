module Ergvein.Types.Utxo
  (
    EgvUtxoSet(..)
  , EgvUtxoStatus(..)
  , BtcUtxoSet
  , EgvUtxoSetStorage
  , BtcUtxoUpdate
  , getBtcUtxoSetFromStore
  , isUtxoConfirmed
  , updateBtcUtxoSetPure
  ) where

import Data.Aeson
import Data.List (foldl')
import Data.Word
import Network.Haskoin.Transaction

import Ergvein.Aeson
import Ergvein.Types.Currency

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data EgvUtxoStatus = EUtxoConfirmed | EUtxoSending | EUtxoReceiving
  deriving (Eq, Show, Read)
$(deriveJSON defaultOptions ''EgvUtxoStatus)

isUtxoConfirmed :: EgvUtxoStatus -> Bool
isUtxoConfirmed v = case v of
  EUtxoConfirmed -> True
  _ -> False

type BtcUtxoSet = M.Map OutPoint (Word64, EgvUtxoStatus)

-- | fst -- wallet's unspent outputs
-- snd -- wallet's spent outputs
-- snd's bool: True - confirmed, must be deleted from UTXO set, False - set status to EUtxoSending
type BtcUtxoUpdate = (BtcUtxoSet, [(OutPoint, Bool)])

data EgvUtxoSet = BtcSet BtcUtxoSet | ErgoSet ()
  deriving (Eq, Show, Read)
$(deriveJSON defaultOptions ''EgvUtxoSet)

instance FromJSONKey OutPoint
instance ToJSONKey OutPoint

type EgvUtxoSetStorage = M.Map Currency EgvUtxoSet

getBtcUtxoSetFromStore :: EgvUtxoSetStorage -> Maybe BtcUtxoSet
getBtcUtxoSetFromStore st = case M.lookup BTC st of
  Just (BtcSet s) -> Just s
  _ -> Nothing

updateBtcUtxoSetPure :: BtcUtxoUpdate -> BtcUtxoSet -> BtcUtxoSet
updateBtcUtxoSetPure (outs, ins) s = foo (M.union outs s) ins $ \m (op, b) ->
  M.update (\(val, _) -> if b then Nothing else Just (val, EUtxoSending)) op m
  where foo b ta f = foldl' f b ta
