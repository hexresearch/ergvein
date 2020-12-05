module Ergvein.Types.Keys.PubKeystore
  (
    PubKeystore(..)
  ) where

import Data.SafeCopy
import Data.Serialize
import Data.Vector
import GHC.Generics

import Ergvein.Types.Keys.Prim
import Ergvein.Types.Keys.EgvPubKeyBox (EgvPubKeyBox(..))

data PubKeystore = PubKeystore {
  pubKeystore'master   :: !EgvXPubKey
  -- ^Extended public key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, pubKeystore'external :: !(Vector EgvPubKeyBox)
  -- ^Map with BIP44 external extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, pubKeystore'internal :: !(Vector EgvPubKeyBox)
  -- ^Map with BIP44 internal extended public keys and corresponding indices.
  -- This addresses must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show, Read, Generic, Serialize)

instance SafeCopy PubKeystore where
  version = 1
  putCopy (PubKeystore m e i) = contain $
    put m >> safePut e >> safePut i
  getCopy = contain $
    PubKeystore <$> get <*> safeGet <*> safeGet
