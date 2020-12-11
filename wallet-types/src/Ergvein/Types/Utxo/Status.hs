module Ergvein.Types.Utxo.Status(
    EgvUtxoStatus(..)
  , isUtxoConfirmed
  ) where

import Data.SafeCopy
import Data.Serialize
import Ergvein.Aeson
import Ergvein.Types.Transaction
import GHC.Generics

data EgvUtxoStatus
  = EUtxoConfirmed
  | EUtxoSemiConfirmed !BlockHeight
  | EUtxoSending !(Maybe BlockHeight)
  | EUtxoReceiving !(Maybe BlockHeight)
  deriving (Eq, Show, Read, Generic)

$(deriveJSON defaultOptions ''EgvUtxoStatus)

instance SafeCopy EgvUtxoStatus where
  version = 1
  putCopy v = contain $ case v of
    EUtxoConfirmed       -> put (0 :: Int)
    EUtxoSemiConfirmed h -> put (1 :: Int) >> put h
    EUtxoSending mh      -> put (2 :: Int) >> put mh
    EUtxoReceiving mh    -> put (3 :: Int) >> put mh
  getCopy = contain $ do
    i :: Int <- get
    case i of
      0 -> pure EUtxoConfirmed
      1 -> EUtxoSemiConfirmed <$> get
      2 -> EUtxoSending <$> get
      3 -> EUtxoReceiving <$> get
      _ -> fail $ "Unknown EgvUtxoStatus tag " <> show i

isUtxoConfirmed :: EgvUtxoStatus -> Bool
isUtxoConfirmed v = case v of
  EUtxoConfirmed -> True
  _ -> False
