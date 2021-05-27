module Ergvein.Types.Transaction.Meta
  ( EgvTxMeta(..)
  ) where

import Data.SafeCopy
import Data.Serialize (put, get)
import Data.Time

import Ergvein.Aeson
import Ergvein.Types.Height
import Ergvein.Types.Orphanage ()

import qualified Network.Haskoin.Block       as HB

data EgvTxMeta = EgvTxMeta {
  etxMetaHeight :: !(Maybe BlockHeight)
, etxMetaHash   :: !(Maybe HB.BlockHash)
, etxMetaTime   :: !UTCTime
} deriving (Eq, Show, Read)

$(deriveJSON (aesonOptionsStripPrefix "etxMeta") ''EgvTxMeta)

instance SafeCopy EgvTxMeta where
  putCopy EgvTxMeta{..} = contain $ do
    put etxMetaHeight
    put etxMetaHash
    put etxMetaTime
  getCopy = contain $ EgvTxMeta <$> get <*> get <*> get
