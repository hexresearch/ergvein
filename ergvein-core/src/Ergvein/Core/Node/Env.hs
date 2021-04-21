module Ergvein.Core.Node.Env(
    NodeEnv(..)
  , NodeM
  , HasNodeEnv(..)
  , newNodeEnv
  , runNode
  ) where

import Control.Monad.Reader
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Types
import Data.Map (Map)
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef
import Sepulcas.Native

data NodeEnv t = NodeEnv {
  nenv'nodeConsRef     :: !(ExternalRef t (ConnMap t))
, nenv'nodeReqSelector :: !(NodeReqSelector t)
, nenv'nodeReqFire     :: !(Map Currency (Map SockAddr NodeMessage) -> IO ())
}

type NodeM t m = ReaderT (NodeEnv t) m

class Monad m => HasNodeEnv t m | m -> t where
  getNodeEnv :: m (NodeEnv t)

instance Monad m => HasNodeEnv t (NodeM t m) where
  getNodeEnv = ask
  {-# INLINE getNodeEnv #-}

instance (HasNodeEnv t m, MonadNodeConstr t m) => MonadNode t m where
  getNodeConnRef = fmap nenv'nodeConsRef getNodeEnv
  {-# INLINE getNodeConnRef #-}
  getNodeNodeReqSelector = fmap nenv'nodeReqSelector getNodeEnv
  {-# INLINE getNodeNodeReqSelector #-}
  getNodeReqFire = fmap nenv'nodeReqFire getNodeEnv
  {-# INLINE getNodeReqFire #-}

newNodeEnv :: (MonadIO m, TriggerEvent t m, Reflex t) => m (NodeEnv t)
newNodeEnv = do
  (nReqE, nReqFire) <- newTriggerEvent
  let nodeSel = fanMap nReqE
  NodeEnv <$> newExternalRef mempty
          <*> pure nodeSel
          <*> pure nReqFire

runNode :: NodeEnv t -> NodeM t m a -> m a
runNode = flip runReaderT
