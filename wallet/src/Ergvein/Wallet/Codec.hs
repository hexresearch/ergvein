{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Wallet.Codec
  (
  ) where

import Network.Haskoin.Block
import Network.Haskoin.Crypto

import Ergvein.Filters.Btc

import qualified Codec.Serialise as S
import qualified Data.Serialize as Cereal

deriving instance S.Serialise BlockHash

instance S.Serialise Hash256 where
  encode = S.encode . Cereal.encode
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ Cereal.decode bs
  {-# INLINE decode #-}

instance S.Serialise BtcAddrFilter where
  encode = S.encode . encodeBtcAddrFilter
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ decodeBtcAddrFilter bs
  {-# INLINE decode #-}

instance S.Serialise BlockNode where
  encode = S.encode . Cereal.encode
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ Cereal.decode bs
  {-# INLINE decode #-}

instance S.Serialise Block where
  encode = S.encode . Cereal.encode
  {-# INLINE encode #-}
  decode = do
    bs <- S.decode
    either fail pure $ Cereal.decode bs
  {-# INLINE decode #-}
