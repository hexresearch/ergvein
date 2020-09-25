module Ergvein.Index.Server.TCPService.Connections 
  ( openConnection
  , closeConnection
  , closeAllConnections
  , newConnection
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Unlift
import Data.Map.Strict (Map(..))
import Data.Maybe
import Ergvein.Index.Protocol.Types (CurrencyCode, Message)
import Ergvein.Index.Server.Dependencies
import Network.Socket

import qualified Data.Map.Strict as Map

openConnection :: HasConnectionsManagement m => ThreadId -> SockAddr -> Socket -> m ()
openConnection threadId addr sock = do
  openedConnectionsRef <- openConnections
  liftIO $ atomically $ modifyTVar openedConnectionsRef $ Map.insert addr (threadId , sock)

newConnection :: SockAddr -> IO (HostName, ServiceName)
newConnection addr = do
  (maybeHost, maybePort) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
  pure (fromJust maybeHost , fromJust maybePort)

closeSocketAndKillThread :: (ThreadId, Socket) -> IO ()
closeSocketAndKillThread (connectionThreadId, connectionSocket) = close connectionSocket >> killThread connectionThreadId

closeConnection :: HasConnectionsManagement m => SockAddr -> m ()
closeConnection addr = do
  openedConnectionsRef <- openConnections
  liftIO $ closeSocketAndKillThread =<< (Map.! addr) <$> readTVarIO openedConnectionsRef

closeAllConnections :: HasConnectionsManagement m => m ()
closeAllConnections = do
  openedConnectionsRef <- openConnections
  liftIO $ do
    traverse closeSocketAndKillThread =<< Map.elems <$> readTVarIO openedConnectionsRef
    atomically $ writeTVar openedConnectionsRef mempty