module Control.Monad.Haskey.Haskoin(

  ) where 

import GHC.Generics
import Data.Binary (Binary(..))
import Data.BTree.Primitives (Value)
import Network.Haskoin.Block
import Network.Haskoin.Crypto
import Data.BTree.Primitives.Key

import qualified Data.Serialize as S

deriving instance Generic BlockHash
deriving instance Generic BlockHeader
deriving instance Generic BlockNode
instance Binary BlockHash
instance Binary BlockHeader
instance Binary BlockNode

instance Binary Hash256 where
  put = put . S.runPut . S.put
  {-# INLINE put #-}
  get = do
    bs <- get
    either fail pure $ S.runGet S.get bs
  {-# INLINE get #-}

instance Value BlockNode
instance Value BlockHash
instance Key BlockHash
