module Ergvein.Index.Server.DB.Serialize.Class
  (
    EgvSerialize(..)
  , getTxHashLength
  , getBlockHashLength
  ) where

import Data.ByteString (ByteString)
import Ergvein.Types.Currency

-- ===========================================================================
--           Custom serialize-deserialize class
-- ===========================================================================

class EgvSerialize a where
  egvSerialize :: Currency -> a -> ByteString
  egvDeserialize :: Currency -> ByteString -> Either String a

getTxHashLength :: Currency -> Int
getTxHashLength cur = case cur of
  ERGO -> 0 --TODO: Add Ergo lengths
  BTC -> 32
{-# INLINE getTxHashLength #-}

getBlockHashLength :: Currency -> Int
getBlockHashLength cur = case cur of
  ERGO -> 0 --TODO: Add Ergo lengths
  BTC -> 32
{-# INLINE getBlockHashLength #-}
