{-# LANGUAGE TypeApplications #-}
-- |
-- Type class API for parsing ergo transactions.
module Data.Ergo.MIR.Parser where

import Control.Monad
import Data.Coerce
import Data.Word
import Data.Persist
import Data.Ergo.Vlq

class ErgoParser a where
  getErgo :: Get a
  -- putErgo :: a -> Put ()



newtype AsVarInt a = AsVarInt a

instance VarInt a => ErgoParser (AsVarInt a) where
  getErgo = coerce @(Get a)       @(Get (AsVarInt a))      decodeVarInt
  -- putErgo = coerce @(a -> Put ()) @(AsVarInt a -> Put ())  encodeVarInt



----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

parseLongListOf :: Get a -> Get [a]
parseLongListOf getV = do
  n <- decodeVarInt @Word32
  replicateM (fromIntegral n) getV


parseListOf :: Get a -> Get [a]
parseListOf getV = do
  n <- decodeVarInt @Word16
  replicateM (fromIntegral n) getV

parseShortListOf :: Get a -> Get [a]
parseShortListOf getV = do
  n <- get @Word8
  replicateM (fromIntegral n) getV
