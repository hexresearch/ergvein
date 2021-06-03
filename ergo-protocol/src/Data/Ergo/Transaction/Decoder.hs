module Data.Ergo.Transaction.Decoder(
    decodeTx
  ) where

import Data.Ergo.Transaction.Types
import Data.ByteString (ByteString)

decodeTx :: ByteString -> Either String Transaction
decodeTx = undefined
