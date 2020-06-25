module Ergvein.Types.Utxo
  (
    EgvUtxoSet(..)
  , EgvUtxoStatus(..)
  , BtcUtxoSet
  , EgvUtxoSetStorage
  , getBtcUtxoSetFromStore
  , isUtxoConfirmed
  , updateBtcUtxoSetPure
  ) where

import Data.Aeson
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

updateBtcUtxoSetPure :: (BtcUtxoSet, [OutPoint]) -> BtcUtxoSet -> BtcUtxoSet
updateBtcUtxoSetPure (outs, ins) s = M.withoutKeys (M.union outs s) (S.fromList ins)
