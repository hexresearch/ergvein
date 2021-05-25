module Ergvein.Types.Height(
      BlockHeight
  ) where

import Data.Word ( Word64 )

-- | Number of blocks before current one, from the starting from Genesis block with height of zero
type BlockHeight = Word64
