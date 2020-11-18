-- |
{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Types.Orphanage where

import Data.Aeson
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short   as BSS
import Network.Haskoin.Transaction (OutPoint)
import Network.Haskoin.Block (BlockHash)
import Ergvein.Text

instance FromJSONKey BlockHash
instance ToJSONKey   BlockHash

instance FromJSONKey OutPoint
instance ToJSONKey   OutPoint

instance FromJSON ShortByteString where
  parseJSON = withText "ShortByteString" $
    either (fail "Failed to parse a ShortByteString") (pure . BSS.toShort) . hex2bsTE
  {-# INLINE parseJSON #-}

instance ToJSON ShortByteString where
  toJSON = String . bs2Hex . BSS.fromShort
  {-# INLINE toJSON #-}
