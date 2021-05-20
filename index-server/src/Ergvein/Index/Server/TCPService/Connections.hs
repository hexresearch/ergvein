module Ergvein.Index.Server.TCPService.Connections 
  ( newConnection
  ) where

import Data.Maybe
import Network.Socket

newConnection :: SockAddr -> IO (HostName, ServiceName)
newConnection addr = do
  (maybeHost, maybePort) <- getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True addr
  pure (fromJust maybeHost , fromJust maybePort)
