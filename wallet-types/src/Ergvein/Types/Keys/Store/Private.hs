module Ergvein.Types.Keys.Store.Private
  (
    PrvKeystore(..)
  ) where

import Data.SafeCopy
import Data.Serialize
import Data.Vector
import GHC.Generics

import Ergvein.Types.Keys.Prim

data PrvKeystore = PrvKeystore {
  prvKeystore'master   :: !EgvXPrvKey
  -- ^Extended private key with BIP44 derivation path /m\/purpose'\/coin_type'\/account'/.
, prvKeystore'external :: !(Vector EgvXPrvKey)
  -- ^Map with BIP44 external extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/0\/address_index/.
, prvKeystore'internal :: !(Vector EgvXPrvKey)
  -- ^Map with BIP44 internal extended private keys and corresponding indices.
  -- This private keys must have the following derivation path:
  -- /m\/purpose'\/coin_type'\/account'\/1\/address_index/.
} deriving (Eq, Show, Read, Generic, Serialize)


instance SafeCopy PrvKeystore where
  version = 1
  putCopy (PrvKeystore m e i) = contain $
    put m >> put e >> put i
  getCopy = contain $
    PrvKeystore <$> get <*> get <*> get
