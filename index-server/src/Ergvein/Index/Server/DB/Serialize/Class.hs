module Ergvein.Index.Server.DB.Serialize.Class
  (
    ErgSerialize(..)
  ) where

import Data.ByteString (ByteString)

-- ===========================================================================
--           Custom serialize-deserialize class
-- ===========================================================================

class ErgSerialize a where
  ergSerialize :: a -> ByteString
  ergDeserialize :: ByteString -> Either String a
