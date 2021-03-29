-- |
-- Type class API for parsing ergo transactions.
module Data.Ergo.MIR.Parser where

import Data.Coerce
import Data.Persist
import Data.Ergo.Vlq

class ErgoParser a where
  getErgo :: Get a
  -- putErgo :: a -> Put ()



newtype AsVarInt a = AsVarInt a

instance VarInt a => ErgoParser (AsVarInt a) where
  getErgo = coerce @(Get a)       @(Get (AsVarInt a))      decodeVarInt
  -- putErgo = coerce @(a -> Put ()) @(AsVarInt a -> Put ())  encodeVarInt
