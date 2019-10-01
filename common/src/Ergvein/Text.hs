module Ergvein.Text(
    showt
  ) where

import Data.Text (Text, pack)

showt :: Show a => a -> Text
showt = pack . show
