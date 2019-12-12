module Ergvein.Interfaces.Ergo.Modifiers.History.ModifierType where

import Data.String
import Data.Serialize                     as S (Serialize (..))
import Data.Word
import Data.Aeson as A

import Ergvein.Interfaces.Ergo.Scorex.Crypto.Hash

newtype TransactionsRoot = TransactionsRoot { unTransactionsRoot :: Digest32 }
  deriving (Eq, Show, IsString, ToJSON, FromJSON, Serialize)

instance HasModifierTypeId TransactionsRoot where
  -- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/scala/org/ergoplatform/modifiers/history/BlockTransactions.scala#L47
  modifierTypeId _ = ModifierTypeId 102

newtype AdProofsRoot = AdProofsRoot { unAdProofsRoot :: Digest32 }
  deriving (Eq, Show, IsString, ToJSON, FromJSON, Serialize)

instance HasModifierTypeId AdProofsRoot where
  -- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/scala/org/ergoplatform/modifiers/history/ADProofs.scala#L73
  modifierTypeId _ = ModifierTypeId 104

newtype ExtensionRoot = ExtensionRoot { unExtensionRoot :: Digest32 }
  deriving (Eq, Show, IsString, ToJSON, FromJSON, Serialize)

instance HasModifierTypeId ExtensionRoot where
  -- https://github.com/ergoplatform/ergo/blob/2ada64922e605bfecccddcbec91af1af52e591b9/src/main/scala/org/ergoplatform/modifiers/history/Extension.scala#L77
  modifierTypeId _ = ModifierTypeId 108

newtype ModifierTypeId = ModifierTypeId { unModifierTypeId :: Word8 }
  deriving (Eq, Show, Serialize)

class HasModifierTypeId a where
  modifierTypeId :: a -> ModifierTypeId
