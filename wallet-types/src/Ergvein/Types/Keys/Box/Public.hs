module Ergvein.Types.Keys.Box.Public
  (
    EgvPubKeyBox(..)
  ) where

import GHC.Generics
import Data.SafeCopy
import Data.Set (Set)
import Data.Serialize

import Ergvein.Types.Keys.Prim
import Ergvein.Types.Transaction

data EgvPubKeyBox = EgvPubKeyBox {
  pubKeyBox'key    :: !EgvXPubKey
, pubKeyBox'txs    :: !(Set TxId)
, pubKeyBox'manual :: !Bool
} deriving (Eq, Show, Read, Generic, Serialize)

instance SafeCopy EgvPubKeyBox where
  version = 1
  putCopy (EgvPubKeyBox k t m) = contain $
    put k >> put t >> put m
  getCopy = contain $
    EgvPubKeyBox <$> get <*> get <*> get
