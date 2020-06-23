module Ergvein.Types.Utxo
  (
    EgvUtxoSet(..)
  , EgvUtxoStatus(..)
  , BtxUtxoSet
  , EgvUtxoSetStorage
  , getBtcUtxoSetFromStore
  , isUtxoConfirmed
  ) where

import Data.Aeson
import Data.Word
import Network.Haskoin.Transaction

import Ergvein.Aeson
import Ergvein.Types.Currency

import qualified Data.Map.Strict as M

data EgvUtxoStatus = EUtxoConfirmed | EUtxoSending | EUtxoReceiving
  deriving (Eq, Show, Read)
$(deriveJSON defaultOptions ''EgvUtxoStatus)

isUtxoConfirmed :: EgvUtxoStatus -> Bool
isUtxoConfirmed v = case v of
  EUtxoConfirmed -> True
  _ -> False

type BtxUtxoSet = M.Map OutPoint (Word64, EgvUtxoStatus)

data EgvUtxoSet = BtcSet BtxUtxoSet | ErgoSet ()
  deriving (Eq, Show, Read)
$(deriveJSON defaultOptions ''EgvUtxoSet)

instance FromJSONKey OutPoint
instance ToJSONKey OutPoint

type EgvUtxoSetStorage = M.Map Currency EgvUtxoSet

getBtcUtxoSetFromStore :: EgvUtxoSetStorage -> Maybe BtxUtxoSet
getBtcUtxoSetFromStore st = case M.lookup BTC st of
  Just (BtcSet s) -> Just s
  _ -> Nothing
